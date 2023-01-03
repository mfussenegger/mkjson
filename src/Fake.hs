{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Fake
  ( State,
    Env (..),
    eval,
    runFakeT,
  )
where

import qualified Aeson as A
import Control.Monad (replicateM)
import Control.Monad.Except (ExceptT, MonadError)
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as State
import Data.Aeson (Value (..), object)
import qualified Data.ByteString.Char8 as BS
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Scientific as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Calendar (Day (..), showGregorian)
import Data.Time.Clock
  ( UTCTime (..),
    diffTimeToPicoseconds,
    picosecondsToDiffTime,
  )
import Data.Time.Format
  ( defaultTimeLocale,
    formatTime,
    parseTimeM,
  )
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUID1
import qualified Data.Vector as V
import Expr (Expr (..), Function (..), pattern Fn)
import System.Random
  ( Random (..),
    RandomGen (..),
  )
import qualified System.Random.Mersenne.Pure64 as Rnd
import qualified Text.Regex.TDFA.Pattern as R
import qualified Text.Regex.TDFA.ReadRegex as R
import Prelude hiding (lines, replicate)
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Pair)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> let exec expr = runFakeT (Just 1) (eval expr)


newtype Fake a = Fake { runFake :: ExceptT String (StateT Env IO) a }
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadState Env
  , MonadIO
  , MonadError String
  )

runFakeT :: Maybe Int -> Fake a -> IO a
runFakeT seed fake = do
  env <- newEnv seed
  result <- State.evalStateT (Except.runExceptT (runFake fake)) env
  case result of
    Left errorMsg -> error errorMsg
    Right result' -> pure result'

type State a = StateT Env IO a

data Env = Env
  { envStdGen :: !Rnd.PureMT
  , envPatternCache :: !(M.HashMap T.Text R.Pattern)
  , envFileCache :: !(M.HashMap T.Text (V.Vector Value)) }

instance RandomGen Env where
  genWord32 env@Env { envStdGen } = let (word32, g') = genWord32 envStdGen
                                    in (word32, env { envStdGen = g' })
  genWord64 env@Env { envStdGen } = let (word64, g') = genWord64 envStdGen
                                    in (word64, env { envStdGen = g' })
  split env = (env { envStdGen = g' }, env { envStdGen = g'' })
    where
      (g', g'') = split (envStdGen env)

newEnv :: Maybe Int -> IO Env
newEnv (Just seed) = pure $ Env (Rnd.pureMT (fromIntegral seed)) M.empty M.empty
newEnv Nothing     = do
  stdgen <- Rnd.newPureMT
  pure $ Env stdgen M.empty M.empty


uuid1 :: IO UUID.UUID
uuid1 = do
  uuid <- UUID1.nextUUID
  case uuid of
    (Just u) -> pure u
    Nothing  -> uuid1


-- | Generate a random int
--
-- >>> exec "randomInt(1, 2)"
-- Number 1.0
randomInt :: Expr -> Expr -> Fake Value
randomInt lower upper = do
  lower' <- Except.liftEither . A.asInt =<< eval lower
  upper' <- Except.liftEither . A.asInt =<< eval upper
  Number . fromIntegral <$> State.state (randomR (lower', upper'))


-- | Generate a random double
--
-- >>> exec "randomDouble(1.5, 3.0)"
-- Number 2.794350179355104
--
-- >>> exec "randomDouble(1.5, 88.0)"
-- Number 76.140860342811
randomDouble :: Expr -> Expr -> Fake Value
randomDouble lower upper = do
  lower' <- Except.liftEither . (S.toRealFloat <$>) . A.asScientific =<< eval lower :: Fake Double
  upper' <- Except.liftEither . (S.toRealFloat <$>) . A.asScientific =<< eval upper :: Fake Double
  Number . S.fromFloatDigits <$> State.state (randomR (lower', upper'))


-- | Generate a random boolean
--
-- >>> exec "randomBool()"
-- Bool False
randomBool :: (RandomGen g, MonadState g m) => m Value
randomBool = Bool <$> State.state random


-- | Select one random item of an array
--
-- >>> exec "oneOf(array(37, 42, 21))"
-- Number 37.0
oneOfArray :: Expr -> Fake Value
oneOfArray arr = do
  arr' <- Except.liftEither . A.asArray =<< eval arr
  idx <- State.state $ randomR (0, length arr' - 1)
  pure $ arr' V.! idx


-- | Select one random argument
--
-- >>> exec "oneOf(37, 42, 21)"
-- Number 37.0
--
-- >>> runFakeT Nothing (oneOfArgs [])
-- *** Exception: At least 1 argument required
-- ...
oneOfArgs :: [Expr] -> Fake Value
oneOfArgs args = do
  rndArg <- rndListItem args
  case rndArg of
    Nothing  -> Except.throwError "At least 1 argument required"
    Just arg -> eval arg


-- | Create an array with `num` items
--
-- >>> exec "replicate(randomInt(2, 4), oneOf(37, 42, 21))"
-- Array [Number 21.0,Number 37.0]
--
replicate :: Expr -> Expr -> Fake Value
replicate num expr = do
  num' <- Except.liftEither . A.asInt =<< eval num
  Array <$> V.replicateM num' (eval expr)


-- | Create an object from a list in the  [key, value [, ...]] form
--
-- >>> exec "object('x', randomInt(2, 4), oneOf('y', 'z'), 3)"
-- Object (fromList [("x",Number 2.0),("y",Number 3.0)])
--
objectFromArgs :: [Expr] -> Fake Value
objectFromArgs args = do
  let
    pairs = fmap (fmap mkKeyValuePair) (mkPairs args)
  Except.liftEither pairs >>= sequence <&> object
  where
    mkPairs [] = Right []
    mkPairs [_] = Left "Arguments to object must be a multiple of 2 (key + value pairs)"
    mkPairs (x : y : rest) = ((x, y) :) <$> mkPairs rest
    mkKeyValuePair :: (Expr, Expr) -> Fake Pair
    mkKeyValuePair (key, val) = do
      key' <- eval key >>= Except.liftEither . A.asText
      val' <- eval val
      pure (fromText key', val')


rndListItem :: (RandomGen g, MonadState g m) => [a] -> m (Maybe a)
rndListItem [] = pure Nothing
rndListItem xs = do
  idx <- State.state $ randomR (0, length xs - 1)
  pure . Just $ xs !! idx


rndSetItem :: (RandomGen g, MonadState g m) => Set.Set a -> m (Maybe a)
rndSetItem xs
  | Set.null xs = pure Nothing
  | otherwise   = do
    idx <- State.state $ randomR (0, Set.size xs - 1)
    pure . Just $ Set.elemAt idx xs


allPossibleChars :: Set.Set Char
allPossibleChars = Set.fromList [minBound..maxBound]


maybeMErr :: MonadError b m => b -> Maybe a -> m a
maybeMErr err Nothing  = Except.throwError err
maybeMErr _   (Just x) = pure x


-- | Create random data that would be matched by the given regex
--
-- >>> exec "fromRegex('\\d-\\d{1,3}-FOO')"
-- String "8-840-FOO"
--
-- >>> exec "fromRegex('[a-z]{3}')"
-- String "iyu"
--
-- >>> exec "fromRegex('[^0-9][0-9]B')"
-- String "\552818\&8B"
--
-- >>> exec "fromRegex('(\\d{4})')"
-- String "8840"
fromRegex :: T.Text -> Fake T.Text
fromRegex input = do
  e@Env{envPatternCache} <- State.get
  case M.lookup input envPatternCache of
    Just pattern' -> generateText pattern'
    Nothing ->
      case R.parseRegex input' of
        Right (pattern', _) -> do
          State.put e { envPatternCache = M.insert input pattern' envPatternCache }
          generateText pattern'
        Left err           -> Except.throwError $ show err
  where
    input' = T.unpack input
    defaultUpper = 10
    replicatePattern lower upper pattern' = do
      numChars <- State.state $ randomR (lower, upper)
      T.concat <$> replicateM numChars (generateText pattern')
    generateText p = case p of
      (R.PGroup _ pattern') -> generateText pattern'
      (R.POr patterns) -> do
        pattern' <- rndListItem patterns
        case pattern' of
          Nothing       -> pure ""
          Just pattern'' -> generateText pattern''
      (R.PConcat patterns) -> T.concat <$> mapM generateText patterns
      (R.PPlus pattern') -> replicatePattern 1 defaultUpper pattern'
      (R.PStar _ pattern') -> replicatePattern 0 defaultUpper pattern'
      (R.PBound lower mUpper pattern') ->
        replicatePattern lower (fromMaybe defaultUpper mUpper) pattern'
      (R.PAny _ patternSet) -> fromPatternSet patternSet
      (R.PAnyNot _ ps@(R.PatternSet mChars _ _ _)) ->
        rndSetItem (maybe Set.empty (Set.difference allPossibleChars) mChars)
        >>= maybeMErr ("Can't generate data from regex pattern: " <> show ps)
        <&> charToText
      (R.PEscape _ 'd') -> T.pack . show <$> State.state (randomR (0, 9 :: Int))
      (R.PChar _ char) -> pure $ charToText char
      _ -> Except.throwError $ "Can't generate data from regex pattern: " <> show p
    fromPatternSet ps@(R.PatternSet mCharSet _ _ _) =
      rndSetItem (fromMaybe Set.empty mCharSet)
      >>= maybeMErr ("Can't generate data from regex pattern: " <> show ps)
      <&> charToText
    charToText c = T.pack [c]


fromFile :: Expr -> Fake Value
fromFile fileName = do
  fileName' <- Except.liftEither . A.asText =<< eval fileName
  e@Env{envFileCache} <- State.get
  case M.lookup fileName' envFileCache of
    (Just lines) -> pure $ Array lines
    Nothing      -> do
      contents <- liftIO $ BS.readFile (T.unpack fileName')
      let
        lines = V.fromList $ fmap (String . T.decodeUtf8) (BS.lines contents)
      State.put e { envFileCache = M.insert fileName' lines envFileCache }
      pure $ Array lines


-- | Generate a random character
--
-- >>> exec "randomChar()"
-- String "\815599"
randomChar :: (RandomGen g, MonadState g m) => m Value
randomChar = charToString <$> State.state random
  where
    charToString :: Char -> Value
    charToString = String . T.pack . (: [])


-- | Generate a random date between (inclusive) lo and hi
--
-- lo and hi default to 1858-11-17 and 2132-09-01
--
-- >>> exec "randomDate()"
-- String "1936-12-18"
--
-- >>> exec "randomDate('2001-01-01', '2018-12-31')"
-- String "2011-10-20"
--
randomDate :: (MonadError String m, RandomGen g, MonadState g m)
           => Maybe T.Text
           -> Maybe T.Text
           -> m Day
randomDate lo hi = randomDate' lo' hi'
  where
    lo' = lo >>= parseDay
    hi' = hi >>= parseDay
    parseDay = parseTimeM False defaultTimeLocale "%F" . T.unpack


randomDate' :: (MonadError String m, RandomGen g,MonadState g m)
            => Maybe Day
            -> Maybe Day
            -> m Day
randomDate' lo hi = ModifiedJulianDay <$> State.state (randomR (lo', hi'))
  where
    lo' = toModifiedJulianDay $ fromMaybe defaultLo lo
    hi' = toModifiedJulianDay $ fromMaybe defaultHi hi
    defaultLo = ModifiedJulianDay 0
    defaultHi = ModifiedJulianDay 100000


-- | Generate a random dateTime
--
-- >>> exec "randomDateTime()"
-- String "1936-12-18T05:21:21Z"
--
-- >>> exec "randomDateTime('2019-10-10', '2019-10-20 17:00')"
-- String "2019-10-18T04:12:09Z"
--
-- >>> exec "randomDateTime('2019-10-10 11:05', '2019-10-10 11:08')"
-- String "2019-10-10T11:06:54Z"
randomDateTime :: (MonadError String m, RandomGen g, MonadState g m)
               => Maybe T.Text
               -> Maybe T.Text
               -> m Value
randomDateTime lo hi = do
  day <- randomDate' loDay hiDay
  pico <- State.state (randomR (loPico, hiPico))
  pure . String . T.pack . formatDateTime $ UTCTime day (picosecondsToDiffTime pico)
  where
    formatDateTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
    lo' = lo >>= parseDateTime
    hi' = hi >>= parseDateTime
    loDay = utctDay <$> lo'
    hiDay = utctDay <$> hi'
    loPico = maybe 0 (diffTimeToPicoseconds. utctDayTime) lo'
    hiPico = maybe 86400000000000000 (diffTimeToPicoseconds . utctDayTime) hi'


-- | Parse a DateTime formatted in iso8601 where the time part is optional
--
-- >>> parseDateTime "2019-10-21"
-- Just 2019-10-21 00:00:00 UTC
--
-- >>> parseDateTime "2019-2-21"
-- Just 2019-02-21 00:00:00 UTC
--
-- >>> parseDateTime "2019-10-21T21:30"
-- Just 2019-10-21 21:30:00 UTC
--
-- >>> parseDateTime "2019-10-21 21:30"
-- Just 2019-10-21 21:30:00 UTC
--
-- >>> parseDateTime "2019-10-21 21:30:59"
-- Just 2019-10-21 21:30:59 UTC
parseDateTime :: T.Text -> Maybe UTCTime
parseDateTime dt = listToMaybe (mapMaybe parse formats)
  where
    formats =
      [ "%Y-%-m-%-d"
      , "%Y-%-m-%-dT%H:%M"
      , "%Y-%-m-%-d %H:%M"
      , "%Y-%-m-%-d %H:%M:%S"
      ]
    dt' = T.unpack dt
    parse :: String -> Maybe UTCTime
    parse format = parseTimeM True defaultTimeLocale format dt'


rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right b) = Just b


dayAsValue :: Day -> Value
dayAsValue = String . T.pack . showGregorian


-- | Create a value getter for an expression
--
-- >>> exec "uuid4()"
-- String "2318fa4e-bb68-4f68-b935-c08e7ae6459a"
--
-- >>> exec "array(randomInt(1, 10), randomDouble(1, 20))"
-- Array [Number 9.0,Number 11.003947581369095]
--
eval :: Expr -> Fake Value
eval (IntLiteral x)    = pure $ Number $ fromInteger x
eval (StringLiteral x) = pure $ String x
eval (DoubleLiteral x) = pure $ Number x
eval (JsonLiteral s) = pure s
eval (Fn "uuid4" []) = String . UUID.toText <$> State.state random
eval (Fn "uuid1" []) = String . UUID.toText <$> liftIO uuid1
eval (Fn "null" []) = pure Null
eval (Fn "randomBool" []) = randomBool
eval (Fn "randomChar" []) = randomChar
eval (Fn "randomInt" []) = randomInt (IntLiteral 0) (IntLiteral 2147483647)
eval (Fn "randomInt" [upper]) = randomInt (IntLiteral 0) upper
eval (Fn "randomInt" [lower, upper]) = randomInt lower upper
eval (Fn "randomDouble" []) = randomDouble (DoubleLiteral 0) (DoubleLiteral 1.7976931348623157E308)
eval (Fn "randomDouble" [upper]) = randomDouble (DoubleLiteral 0) upper
eval (Fn "randomDouble" [lower, upper]) = randomDouble lower upper
eval (Fn "randomDate" []) = dayAsValue <$> randomDate Nothing Nothing
eval (Fn "randomDate" [lower, upper]) = do
  lo <- A.asText <$> eval lower
  hi <- A.asText <$> eval upper
  dayAsValue <$> randomDate (rightToMaybe lo) (rightToMaybe hi)
eval (Fn "randomDateTime" []) = randomDateTime Nothing Nothing
eval (Fn "randomDateTime" [lower, upper]) = do
  lower' <- rightToMaybe . A.asText <$> eval lower
  upper' <- rightToMaybe . A.asText <$> eval upper
  randomDateTime lower' upper'
eval (Fn "array" args) = Array . V.fromList <$> mapM eval args
eval (Fn "oneOf" [arg]) = oneOfArray arg
eval (Fn "oneOf" args) = oneOfArgs args
eval (Fn "replicate" [num, expr]) = replicate num expr
eval (Fn "object" args) = objectFromArgs args
eval (Fn "fromFile" [fileName]) = fromFile fileName
eval (Fn "fromRegex" [pattern']) =
  eval pattern'
  >>= Except.liftEither . A.asText
  >>= fromRegex
  <&> String
eval (FunctionCall (Function name _)) = Except.throwError $ "No random generator for " <> T.unpack name
