{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad              (forever)
import           Control.Monad.IO.Class     (liftIO)
import qualified Control.Monad.State.Strict as State
import           Data.Aeson                 (encode, object)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Either                (isRight, lefts)
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Expr                       (parseExpr)
import           Fake                       (eval, newEnv)
import           System.Environment         (getArgs)

parseColumnDefinition :: String -> Maybe (Text, Text)
parseColumnDefinition x =
  case parts of
    [columnName, providerName] -> Just (columnName, providerName)
    _                          -> Nothing
  where
    text = T.pack x
    parts = T.splitOn "=" text


main :: IO ()
main = do
  args <- getArgs
  env <- newEnv Nothing
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
        State.runStateT printRecords env >> pure ()
    else
      mapM_ print errored
  where
    unpackRight (x, Right y) = (x, y)
    unpackRight _            = error "Tuple must only contain Right eithers"
    runProvider (column, provider) = do
      val <- provider
      pure (column, val)
