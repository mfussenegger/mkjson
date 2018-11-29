{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad              (forM_, forever)
import           Data.Aeson                 (Value (..), encode, object)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List                  as L
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Read             as T
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID4
import           Lib
import           System.Environment         (getArgs)


parseColumnDefinition :: String -> Maybe (Text, Text)
parseColumnDefinition x = 
  case parts of
    [x, y]  -> Just (x, y)
    _       -> Nothing
  where
    text = T.pack x
    parts = T.splitOn "=" text


lookupProvider :: Text -> IO Value
lookupProvider "uuid4"            = String . UUID.toText <$> UUID4.nextRandom
lookupProvider _                  = pure $ String "foobar"


main :: IO ()
main = do
  args <- getArgs
  let 
    columns = mapMaybe parseColumnDefinition args
    providers = fmap (\(x, y) -> (x, lookupProvider y)) columns
  forever $ do
    obj <- encode . object <$> mapM runProvider providers
    BL.putStrLn obj
  where
    runProvider (column, provider) = do
      val <- provider
      pure (column, val)
