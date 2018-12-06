{-# LANGUAGE OverloadedStrings #-}

module Expr where

import           Data.Maybe        (fromMaybe)
import           Data.String       (IsString (..))
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Text.Parsec       (many, many1, optionMaybe, parse,
                                    sepBy, (<|>))
import           Text.Parsec.Char  (char, digit, letter, spaces, noneOf)
import           Text.Parsec.Error (ParseError)
import           Text.Parsec.Text  (Parser)

-- $setup
-- >>> :set -XOverloadedStrings

data Expr = IntLiteral Integer
          | StringLiteral Text
          | FunctionCall { fcName :: Text, fcArgs :: [Expr] }
          deriving (Show, Eq)


instance IsString Expr where
  fromString s =
    case parseExpr (T.pack s) of
      (Left err) -> error $ show err
      (Right e)  -> e

expr :: Parser Expr
expr = literal <|> functionCall

literal :: Parser Expr
literal = number <|> stringLiteral

number :: Parser Expr
number = IntLiteral . read <$> many1 digit

stringLiteral :: Parser Expr
stringLiteral = do
  _ <- char '\''
  content <- many (noneOf "\'")
  _ <- char '\''
  pure $ StringLiteral $ T.pack content


functionCall :: Parser Expr
functionCall = do
  name <- ident
  args <- fromMaybe [] <$> optionMaybe functionArgs
  pure $ FunctionCall name args
  where
    functionArgs = do
      _ <- char '('
      args <- expr `sepBy` (char ',' >> spaces)
      _ <- char ')'
      pure args


ident :: Parser Text
ident = do
  firstChar <- letter
  next <- many (digit <|> letter)
  pure $ T.pack (firstChar : next)



-- | Parse an expression
--
-- >>> parseExpr "20"
-- Right (IntLiteral 20)
--
-- >>> parseExpr "uuid4"
-- Right (FunctionCall {fcName = "uuid4", fcArgs = []})
--
-- >>> parseExpr "randomInt(0, 10)"
-- Right (FunctionCall {fcName = "randomInt", fcArgs = [IntLiteral 0,IntLiteral 10]})
--
-- >>> parseExpr "'fileName-200.txt'"
-- Right (StringLiteral "fileName-200.txt")
--
-- >>> parseExpr "''"
-- Right (StringLiteral "")
parseExpr :: Text -> Either ParseError Expr
parseExpr = parse expr "(unknown)"
