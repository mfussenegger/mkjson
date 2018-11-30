{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad              (forever)
import           Data.Aeson                 (Value (..), encode, object)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Either                (isRight, lefts)
import           Data.Maybe                 (fromJust, mapMaybe)
import qualified Data.Scientific            as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Read             as T
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V1               as UUID1
import qualified Data.UUID.V4               as UUID4
import           Expr                       (Expr (..), parseExpr)
import           System.Environment         (getArgs)
import           System.Random              (getStdGen, randomR, setStdGen)

-- $setup
-- >>> :set -XOverloadedStrings

parseColumnDefinition :: String -> Maybe (Text, Text)
parseColumnDefinition x =
  case parts of
    [columnName, providerName] -> Just (columnName, providerName)
    _                          -> Nothing
  where
    text = T.pack x
    parts = T.splitOn "=" text


uuid1 :: IO UUID.UUID
uuid1 = do
  uuid <- UUID1.nextUUID
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


-- | Create a value getter for an expression
--
-- >>> eval $ FunctionCall "randomInt" [IntLiteral 1, IntLiteral 1]
-- Number 1.0
eval :: Expr -> IO Value
eval (IntLiteral x)    = pure $ Number $ fromInteger x
eval (StringLiteral x) = pure $ String x
eval (FunctionCall "uuid4" []) = String . UUID.toText <$> UUID4.nextRandom
eval (FunctionCall "uuid1" []) = String . UUID.toText <$> uuid1
eval (FunctionCall "randomInt" [lower, upper]) = do
  lower' <- asInt <$> eval lower
  upper' <- asInt <$> eval upper
  stdGen <- getStdGen
  let
    (rndNumber, newStdGen) = randomR (lower', upper') stdGen
  setStdGen newStdGen
  pure $ Number $ fromIntegral rndNumber
eval (FunctionCall name _) = pure $ String $ "No random generator for " <> name


main :: IO ()
main = do
  args <- getArgs
  let 
    columns = mapMaybe parseColumnDefinition args
    allExpressions = fmap (\(x, y) -> (x, parseExpr y)) columns
    expressions = fmap unpackRight (filter (isRight . snd) allExpressions)
    errored = lefts $ fmap snd allExpressions
    providers = fmap (\(x, y) -> (x, eval y)) expressions
  if null errored
    then forever $
      mapM runProvider providers >>= BL.putStrLn . encode . object
    else
      mapM_ print errored
  where
    unpackRight (x, Right y) = (x, y)
    unpackRight _            = error "Tuple must only contain Right eithers"
    runProvider (column, provider) = do
      val <- provider
      pure (column, val)
