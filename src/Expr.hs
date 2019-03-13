{-# LANGUAGE OverloadedStrings #-}

module Expr where

import           Data.Aeson              (Value (..), decode', encode)
import           Data.Scientific         (Scientific)
import           Data.String             (IsString (..))
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Text.Parsec             (between, many, many1, optionMaybe,
                                          parse, sepBy, (<|>))
import           Text.Parsec.Char        (char, digit, letter, noneOf, spaces)
import           Text.Parsec.Error       (ParseError)
import           Text.Parsec.Text        (Parser)

-- $setup
-- >>> :set -XOverloadedStrings

data Expr = IntLiteral !Integer
          | DoubleLiteral !Scientific
          | StringLiteral !Text
          | JsonLiteral !Value
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
literal = number <|> stringLiteral <|> jsonLiteral


showExpr :: Expr -> Text
showExpr (StringLiteral s) = "\"" <> s <> "\""
showExpr (IntLiteral n)    = T.pack . show $ n
showExpr (DoubleLiteral n) = T.pack . show $ n
showExpr (JsonLiteral s)   = TL.toStrict . TL.decodeUtf8 $ encode s
showExpr (FunctionCall _)  = error "Can only convert literals to text representation"


jsonLiteral :: Parser Expr
jsonLiteral = do
  assignments <- between (char '{') (char '}') (assignment `sepBy` comma)
  let
    objStr = "{" <> T.intercalate ", " assignments <> "}"
  case decode' (TL.encodeUtf8 . TL.fromStrict $ objStr) of
    Nothing -> error $ "Invalid JSON string: " <> T.unpack objStr
    Just v  -> pure $ JsonLiteral v
  where
    assignment = do
      key <- stringLiteral 
      _ <- colon
      value <- literal
      pure $ showExpr key <> ": " <> showExpr value
    colon = char ':' >> spaces
    comma = char ',' >> spaces


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
    quote = char '\'' <|> char '"'
    string = between quote quote (many (noneOf "\'\""))


functionCall :: Parser Expr
functionCall = do
  name <- ident
  args <- optionMaybe functionArgs
  case args of
    Nothing    -> pure $ StringLiteral name
    Just args' -> pure $ FunctionCall (Function name args')
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
-- >>> parseExpr "uuid4()"
-- Right (FunctionCall (Function {fcName = "uuid4", fcArgs = []}))
--
-- >>> parseExpr "uuid4"
-- Right (StringLiteral "uuid4")
--
-- >>> parseExpr "randomInt(0, 10)"
-- Right (FunctionCall (Function {fcName = "randomInt", fcArgs = [IntLiteral 0,IntLiteral 10]}))
--
-- >>> parseExpr "'fileName-200.txt'"
-- Right (StringLiteral "fileName-200.txt")
--
-- >>> parseExpr "''"
-- Right (StringLiteral "")
--
-- >>> parseExpr "{}"
-- Right (JsonLiteral (Object (fromList [])))
--
-- >>> parseExpr "{\"x\": 10, \"y\": {}}"
-- Right (JsonLiteral (Object (fromList [("x",Number 10.0),("y",Object (fromList []))])))
parseExpr :: Text -> Either ParseError Expr
parseExpr = parse expr "(unknown)"
