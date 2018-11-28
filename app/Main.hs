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
import qualified Faker.Address              as F
import qualified Faker.Business             as F
import qualified Faker.Internet             as F
import qualified Faker.Lorem                as F
import qualified Faker.Name                 as F
import qualified Faker.Utils                as F
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
lookupProvider "firstName"        = String . T.pack <$> F.firstName
lookupProvider "lastName"         = String . T.pack <$> F.lastName
lookupProvider "prefix"           = String . T.pack <$> F.prefix
lookupProvider "suffix"           = String . T.pack <$> F.suffix
lookupProvider "name"             = String . T.pack <$> F.name
lookupProvider "title"            = String . T.pack <$> F.title
lookupProvider "city"             = String . T.pack <$> F.city
lookupProvider "streetName"       = String . T.pack <$> F.streetName
lookupProvider "streetAddress"    = String . T.pack <$> F.streetAddress
lookupProvider "latitude"         = Number . read <$> F.latitude
lookupProvider "longitude"        = Number . read <$> F.longitude
lookupProvider "cityPrefix"       = String . T.pack <$> F.cityPrefix
lookupProvider "citySuffix"       = String . T.pack <$> F.citySuffix
lookupProvider "country"          = String . T.pack <$> F.country
lookupProvider "countryCode"      = String . T.pack <$> F.countryCode
lookupProvider "buildingNumber"   = Number . read <$> F.buildingNumber
lookupProvider "streetSuffix"     = String . T.pack <$> F.streetSuffix
lookupProvider "secondaryAddress" = String . T.pack <$> F.secondaryAddress
lookupProvider "postcode"         = String . T.pack <$> F.postcode
lookupProvider "postcodeByState"  = String . T.pack <$> F.postcodeByState
lookupProvider "state"            = String . T.pack <$> F.state
lookupProvider "stateAbbr"        = String . T.pack <$> F.stateAbbr
lookupProvider "timeZone"         = String . T.pack <$> F.timeZone
lookupProvider "email"            = String . T.pack <$> F.email
lookupProvider "userName"         = String . T.pack <$> F.userName
lookupProvider "creditCardNumber" = String . T.pack <$> F.creditCardNumber
lookupProvider "creditCardType"   = String . T.pack <$> F.creditCardType
lookupProvider "randomNum"        = Number . fromIntegral <$> F.randomNum (0, maxBound)
lookupProvider "uuid4"            = String . UUID.toText <$> UUID4.nextRandom
lookupProvider _                  = String . T.pack <$> F.word


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
