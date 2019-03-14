
module Main where

import qualified Cli
import           Control.Monad              (forever, replicateM)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (encode, object)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Fake                       (eval, runFakeT)
import           System.Environment         (getArgs)


main :: IO ()
main = do
  args <- getArgs
  parsedArgs <- Cli.parseArgs args
  let
    fields = fmap (\(x, y) -> (x, eval y)) (Cli.fields parsedArgs)
    printRecords = loop (Cli.num parsedArgs) $
      mapM evalField fields >>= liftIO . BL.putStrLn . encode . object
  runFakeT (Cli.seed parsedArgs) printRecords >> pure ()
  where
    loop Cli.Infinite  = forever
    loop (Cli.Const n) = replicateM n
    evalField (column, fakeExpr) = do
      val <- fakeExpr
      pure (column, val)
