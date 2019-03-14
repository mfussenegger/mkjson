{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Cli
  ( Args(..)
  , Amount(..)
  , parseArgs
  ) where


import           Data.Bifunctor      (bimap)
import qualified Data.Text           as T
import qualified Expr                as E
import           Options.Applicative hiding (Const)
import           Text.Read           (readMaybe)


type Field = (T.Text, E.Expr)


data Args = Args
  { seed   :: !(Maybe Int)
  , num    :: !Amount
  , fields :: ![Field] }
  deriving (Show)


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


parseExpr :: String -> Either String Field
parseExpr s =
  case parts of
    [field, expr] -> bimap show (field ,) (E.parseExpr expr)
    _             -> Left "Field must be in <name>=<expr> format"
  where
    parts = T.splitOn "=" (T.pack s)


-- | Generate field completions
--
-- >>> completeField "x=randomI"
-- ["randomInt("]
--
-- >>> completeField "x"
-- ["="]
completeField :: String -> [String]
completeField prefix =
  case parts of
    [_, suffix] -> map T.unpack $ filter (T.isPrefixOf suffix) providers
    [_]         -> ["="]
    _           -> []
  where
    prefix' = T.pack prefix
    parts = T.splitOn "=" prefix'
    providers =
      [ "randomInt("
      , "randomBoolean()" ]


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
  <*> many (argument (eitherReader parseExpr)
    ( completer (mkCompleter (idm completeField))
    <> metavar "FIELDS..."
    <> help "The fields the records should have in <fieldName>=<expression> format." ))


parseArgs :: IO Args
parseArgs = execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc <> progDesc "Generate (random) JSON records" )
