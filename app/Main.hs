{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where


import qualified Aeson                            as A
import           Control.Monad                    (forM, forever)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Aeson                       (Value (..), encode, object)
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy.Char8       as BL
import           Data.Either                      (isRight, lefts)
import qualified Data.HashMap.Strict              as M
import           Data.Maybe                       (mapMaybe)
import qualified Data.Scientific                  as S
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.UUID                        as UUID
import qualified Data.UUID.V1                     as UUID1
import qualified Data.Vector                      as V
import           Expr                             (Expr (..), parseExpr)
import           Prelude                          hiding (lines)
import           System.Environment               (getArgs)
import           System.Random                    (StdGen, newStdGen, random,
                                                   randomR)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import System.Random (mkStdGen)

type State a = StateT Env IO a


data Env = Env
  { envStdGen :: StdGen
  , envFileCache :: M.HashMap Text (V.Vector Value) }


withStdGen :: (StdGen -> (a, StdGen)) -> State a
withStdGen f = do
  e@Env{..} <- State.get
  let
    (x, stdGen) = f envStdGen
  State.put $ e { envStdGen = stdGen }
  pure x


parseColumnDefinition :: String -> Maybe (Text, Text)
parseColumnDefinition x =
  case parts of
    [columnName, providerName] -> Just (columnName, providerName)
    _                          -> Nothing
  where
    text = T.pack x
    parts = T.splitOn "=" text


uuid1 :: State UUID.UUID
uuid1 = do
  uuid <- liftIO UUID1.nextUUID
  case uuid of
    (Just u) -> pure u
    Nothing  -> uuid1


-- | Create a value getter for an expression
--
-- >>> let g = Env (mkStdGen 1) M.empty
-- >>> let exec expr = State.evalStateT (eval expr) g
--
-- >>> exec "randomInt(1, 2)"
-- Number 2.0
--
-- >>> exec "uuid4"
-- String "0099a82c-36f7-4321-8012-daa4305fd84b"
--
-- >>> exec "array(randomInt(1, 10), randomDouble(1, 20))"
-- Array [Number 6.0,Number 14.108305934824038]
--
-- >>> exec "oneOf(array(37, 42, 21))"
-- Number 21.0
--
-- >>> exec "oneOf(37, 42, 21)"
-- Number 21.0
--
-- >>> exec "replicate(randomInt(2, 4), oneOf(37, 42, 21))"
-- Array [Number 42.0,Number 42.0,Number 21.0,Number 42.0]
--
-- >>> exec "object('x', randomInt(2, 4), oneOf('y', 'z'), 3)"
-- Object (fromList [("x",Number 4.0),("y",Number 3.0)])
eval :: Expr -> State Value
eval (IntLiteral x)    = pure $ Number $ fromInteger x
eval (StringLiteral x) = pure $ String x
eval (FunctionCall "uuid4" []) = String . UUID.toText <$> withStdGen random
eval (FunctionCall "uuid1" []) = String . UUID.toText <$> uuid1
eval (FunctionCall "randomInt" [lower, upper]) = do
  lower' <- A.asInt <$> eval lower
  upper' <- A.asInt <$> eval upper
  Number . fromIntegral <$> withStdGen (randomR (lower', upper'))
eval (FunctionCall "randomDouble" [lower, upper]) = do
  lower' <- A.asDouble <$> eval lower
  upper' <- A.asDouble <$> eval upper
  Number . S.fromFloatDigits <$> withStdGen (randomR (lower', upper'))
eval (FunctionCall "array" args) = Array . V.fromList <$> mapM eval args
eval (FunctionCall "oneOf" [arg]) = do
  arr <- A.asArray <$> eval arg
  idx <- withStdGen $ randomR (0, length arr - 1)
  pure $ V.unsafeIndex arr idx
eval (FunctionCall "oneOf" args) = do
  idx <- withStdGen $ randomR (0, length args - 1)
  eval (args !! idx)
eval (FunctionCall "replicate" [num, expr]) = do
  num' <- A.asInt <$> eval num
  Array <$> V.replicateM num' (eval expr)
eval (FunctionCall "object" args) = do
  let
    keyValuePairs = mkPairs (fmap eval args)
    mkPairs [] = []
    mkPairs [_] = error "Arguments to object must be a multiple of 2 (key + value pairs)"
    mkPairs (x : y : rest) = (x, y) : mkPairs rest
  pairs <- forM keyValuePairs (\(key, val) -> do
    key' <- A.asText <$> key
    val' <- val
    pure (key', val'))
  pure $ object pairs
eval (FunctionCall "fromFile" [fileName]) = do
  fileName' <- A.asText <$> eval fileName
  e@Env{..} <- State.get
  case M.lookup fileName' envFileCache of
    (Just lines) -> pure $ Array lines
    Nothing      -> do
      contents <- liftIO $ BS.readFile (T.unpack fileName')
      let
        lines = V.fromList $ fmap (String . T.decodeUtf8) (BS.lines contents)
      State.put e { envFileCache = M.insert fileName' lines envFileCache }
      pure $ Array lines
eval (FunctionCall name _) = pure $ String $ "No random generator for " <> name


main :: IO ()
main = do
  args <- getArgs
  stdGen <- newStdGen
  let
    columns = mapMaybe parseColumnDefinition args
    allExpressions = fmap (\(x, y) -> (x, parseExpr y)) columns
    expressions = fmap unpackRight (filter (isRight . snd) allExpressions)
    errored = lefts $ fmap snd allExpressions
    providers = fmap (\(x, y) -> (x, eval y)) expressions
    env = Env stdGen M.empty
  if null errored
    then
      let
        printRecords = forever $
          mapM runProvider providers >>= liftIO . BL.putStrLn . encode . object
      in
        State.runStateT printRecords env >> pure ()
    else
      mapM_ print errored
  where
    unpackRight (x, Right y) = (x, y)
    unpackRight _            = error "Tuple must only contain Right eithers"
    runProvider (column, provider) = do
      val <- provider
      pure (column, val)
