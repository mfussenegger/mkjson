{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cli
  ( Args(..)
  , Amount(..)
  , parseArgs
  , Field
  ) where


import           Data.Bifunctor      (bimap)
import qualified Data.Text           as T
import qualified Expr                as E
import           Options.Applicative hiding (Const)
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import Data.Functor (($>))


type Field = (T.Text, E.Expr)


data Args = Args
  { seed   :: !(Maybe Int)
  , num    :: !Amount
  , fields :: ![Field] }
  deriving (Show)


data Amount = Const Int
            | Infinite
            deriving (Show)


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right


-- >>> P.parse numParser "<num>" "Inf"
-- Right Infinite
--
-- >>> P.parse numParser "<num>" "200"
-- Right (Const 200)
--
-- >>> P.parse numParser "<num>" "5e4"
-- Right (Const 50000)
numParser :: P.Parser Amount
numParser = inf <|> integer
  where
    inf = P.string "Inf" $> Infinite
    integer = do
      digits <- read @Int <$> P.many1 P.digit
      power <- P.optionMaybe eNotation
      pure . Const $ digits * maybe 1 (10 ^) power
    eNotation :: P.Parser Int
    eNotation = read @Int <$> (P.oneOf "eE" >> P.many1 P.digit)


parseNum :: ReadM Amount
parseNum = eitherReader $ \s -> mapLeft show $ P.parse numParser s s


parseExpr :: String -> Either String Field
parseExpr s =
  case parts of
    [field, expr] -> bimap show (field ,) (E.parseExpr expr)
    [field]       -> bimap show (field ,) (E.parseExpr "null()")
    _             -> Left "Field must be in <name>=<expr> format"
  where
    parts = T.splitOn "=" (T.pack s)


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
  <*> many (argument (eitherReader parseExpr) (metavar "FIELDS..."))


parseArgs :: [String] -> IO Args
parseArgs cliArgs = handleParseResult $ execParserPure defaultPrefs opts cliArgs
  where
    opts = info (args <**> helper)
      ( fullDesc <> progDesc
      "Generate random JSON records. Use <field>=<provider> pairs to specify the fields the records should have." )
