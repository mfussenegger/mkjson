{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad                    (forever)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Aeson                       (Value (..), encode, object)
import qualified Data.ByteString.Lazy.Char8       as BL
import           Data.Either                      (isRight, lefts)
import           Data.Maybe                       (fromJust, mapMaybe)
import qualified Data.Scientific                  as S
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Read                   as T
import qualified Data.UUID                        as UUID
import qualified Data.UUID.V1                     as UUID1
import           Expr                             (Expr (..), parseExpr)
import           System.Environment               (getArgs)
import           System.Random                    (StdGen, newStdGen, random,
                                                   randomR)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import System.Random (mkStdGen)

type State a = StateT StdGen IO a


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


-- | Try to extract an Int from Value
--
-- >>> asInt (Number 10)
-- 10
--
-- >>> asInt (String "10")
-- 10
--
-- >>> asInt (String "foo")
-- *** Exception: Expected an integer, but received: foo
-- ...
asInt :: Value -> Int
asInt (Number n) = fromJust $ S.toBoundedInteger n
asInt (String s) =
  case T.decimal s of
    (Right (n, _)) -> n
    (Left _)       -> error $ "Expected an integer, but received: " <> T.unpack s
asInt o          = error $ "Expected an integer but received: " <> show o


withGen :: (StdGen -> (a, StdGen)) -> State a
withGen f = do
  stdGen <- State.get
  let
    (x, newGen) = f stdGen
  State.put newGen
  pure x


-- | Create a value getter for an expression
--
-- >>> State.evalStateT (eval "randomInt(1, 2)") (mkStdGen 1)
-- Number 2.0
--
-- >>> State.evalStateT (eval "uuid4") (mkStdGen 1)
-- String "0099a82c-36f7-4321-8012-daa4305fd84b"
eval :: Expr -> State Value
eval (IntLiteral x)    = pure $ Number $ fromInteger x
eval (StringLiteral x) = pure $ String x
eval (FunctionCall "uuid4" []) = String . UUID.toText <$> withGen random
eval (FunctionCall "uuid1" []) = String . UUID.toText <$> uuid1
eval (FunctionCall "randomInt" [lower, upper]) = do
  lower' <- asInt <$> eval lower
  upper' <- asInt <$> eval upper
  Number . fromIntegral <$> withGen (randomR (lower', upper'))
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
  if null errored
    then
      let
        printRecords = forever $
          mapM runProvider providers >>= liftIO . BL.putStrLn . encode . object
      in
        State.runStateT printRecords stdGen >> pure ()
    else
      mapM_ print errored
  where
    unpackRight (x, Right y) = (x, y)
    unpackRight _            = error "Tuple must only contain Right eithers"
    runProvider (column, provider) = do
      val <- provider
      pure (column, val)
