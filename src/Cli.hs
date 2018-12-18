
module Cli
  ( Args(..)
  , parseArgs
  ) where


import Options.Applicative

newtype Args = Args
  { seed :: Maybe Int }

args :: Parser Args
args = Args
  <$> optional 
    ( option auto
      ( long "seed"
      <> help "A seed for the random data generator"
      <> metavar "INT" ))

parseArgs :: [String] -> IO Args
parseArgs cliArgs = handleParseResult $ execParserPure defaultPrefs opts cliArgs
  where
    opts = info (args <**> helper)
      ( fullDesc <> progDesc 
      "Generate random JSON records. Use <field>=<provider> pairs to specify the fields the records should have." )
