{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Cli
import           Control.Monad              (forever, replicateM)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (encode, object)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Either                (isRight, lefts)
import           Data.List                  (partition)
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Expr                       (parseExpr)
import           Fake                       (eval, runFakeT)
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
  let
    (cliArgs, fields) = partition (notElem '=') args
    columns = mapMaybe parseColumnDefinition fields
    allExpressions = fmap (\(x, y) -> (x, parseExpr y)) columns
    expressions = fmap unpackRight (filter (isRight . snd) allExpressions)
    errored = lefts $ fmap snd allExpressions
    providers = fmap (\(x, y) -> (x, eval y)) expressions
  parsedArgs <- Cli.parseArgs cliArgs
  if null errored
    then
      let
        printRecords = loop (Cli.num parsedArgs) $
          mapM runProvider providers >>= liftIO . BL.putStrLn . encode . object
      in
        runFakeT (Cli.seed parsedArgs) printRecords >> pure ()
    else
      mapM_ print errored
  where
    loop Cli.Infinite  = forever
    loop (Cli.Const n) = replicateM n
    unpackRight (x, Right y) = (x, y)
    unpackRight _            = error "Tuple must only contain Right eithers"
    runProvider (column, provider) = do
      val <- provider
      pure (column, val)
