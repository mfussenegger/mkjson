{-# LANGUAGE OverloadedStrings #-}

module Expr where

import           Data.Maybe        (fromMaybe)
import           Data.Scientific   (Scientific)
import           Data.String       (IsString (..))
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Text.Parsec       (many, many1, optionMaybe, parse, sepBy,
                                    (<|>), between)
import           Text.Parsec.Char  (char, digit, letter, noneOf, spaces)
import           Text.Parsec.Error (ParseError)
import           Text.Parsec.Text  (Parser)

-- $setup
-- >>> :set -XOverloadedStrings

data Expr = IntLiteral !Integer
          | DoubleLiteral !Scientific
          | StringLiteral !Text
          | FunctionCall !Function
          deriving (Show, Eq)


data Function = Function
  { fcName :: !Text
  , fcArgs :: ![Expr] }
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
number = do
  integer <- many1 digit
  optionalDot <- optionMaybe (char '.')
  case optionalDot of
    Nothing  -> pure $ IntLiteral (read integer)
    (Just _) -> do
      fractional <- many digit
      let
        value = read $ integer <> "." <> fractional
      pure $ DoubleLiteral value


stringLiteral :: Parser Expr
stringLiteral = StringLiteral . T.pack <$> string
  where
    singleQuote = char '\''
    string = between singleQuote singleQuote (many (noneOf "\'"))


functionCall :: Parser Expr
functionCall = do
  name <- ident
  args <- fromMaybe [] <$> optionMaybe functionArgs
  pure $ FunctionCall (Function name args)
  where
    functionArgs = between open close (expr `sepBy` comma)
    open = char '('
    close = char ')'
    comma = char ',' >> spaces


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
-- >>> parseExpr "20.3"
-- Right (DoubleLiteral 20.3)
--
-- >>> parseExpr "uuid4"
-- Right (FunctionCall (Function {fcName = "uuid4", fcArgs = []}))
--
-- >>> parseExpr "randomInt(0, 10)"
-- Right (FunctionCall (Function {fcName = "randomInt", fcArgs = [IntLiteral 0,IntLiteral 10]}))
--
-- >>> parseExpr "'fileName-200.txt'"
-- Right (StringLiteral "fileName-200.txt")
--
-- >>> parseExpr "''"
-- Right (StringLiteral "")
parseExpr :: Text -> Either ParseError Expr
parseExpr = parse expr "(unknown)"
