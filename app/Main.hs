{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import qualified Cli
import Control.Monad (forever, replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object)
import Data.Aeson.Key (fromText)
import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy.Char8 as BL
import Fake (eval, runFakeT)
import ObjectGroups (mergeObjects)
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  parsedArgs <- Cli.parseArgs args
  let
    fields = mergeObjects $ Cli.fields parsedArgs
    fakeFields = fmap (second eval) fields
    printRecords = loop (Cli.num parsedArgs) $
      mapM evalField fakeFields >>= liftIO . BL.putStrLn . encode . object
  runFakeT (Cli.seed parsedArgs) printRecords >> pure ()
  where
    loop Cli.Infinite  = forever
    loop (Cli.Const n) = replicateM n
    evalField (column, fakeExpr) = do
      val <- fakeExpr
      pure (fromText column, val)
