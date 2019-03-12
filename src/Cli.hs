
module Cli
  ( Args(..)
  , Amount(..)
  , parseArgs
  ) where


import           Options.Applicative hiding (Const)
import           Text.Read           (readMaybe)


data Args = Args
  { seed :: !(Maybe Int)
  , num :: !Amount }

data Amount = Const Int
            | Infinite
            deriving (Show)


parseNum :: ReadM Amount
parseNum = eitherReader parseNum'
  where
    parseNum' "Inf" = Right Infinite
    parseNum' s     = case readMaybe s of
      (Just n) -> Right (Const n)
      Nothing  -> Left "Expected a number or `Inf` for infinite records"


args :: Parser Args
args = Args
  <$> optional 
    ( option auto
      ( long "seed"
      <> help "A seed for the random data generator"
      <> metavar "INT" ))
  <*> option parseNum
    ( long "num"
    <> value (Const 1)
    <> help "Number of records to generate. Use `Inf` for infinite records" )


parseArgs :: [String] -> IO Args
parseArgs cliArgs = handleParseResult $ execParserPure defaultPrefs opts cliArgs
  where
    opts = info (args <**> helper)
      ( fullDesc <> progDesc 
      "Generate random JSON records. Use <field>=<provider> pairs to specify the fields the records should have." )
