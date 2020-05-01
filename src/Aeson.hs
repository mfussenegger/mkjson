
module Aeson (
  asInt,
  asText,
  asScientific,
  asArray
) where


import Data.Aeson (Value (..))
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import Text.Read (readMaybe)

-- | Try to extract an Int from Value
--
-- >>> asInt (Number 10)
-- Right 10
--
-- >>> asInt (String "10")
-- Right 10
--
-- >>> asInt (Number 10.38)
-- Left "Could not convert 10.38 to an Int"
--
-- >>> asInt (String "foo")
-- Left "input does not start with a digit"
asInt :: Value -> Either String Int
asInt (Number n) = case S.toBoundedInteger n of
  Nothing -> Left $ "Could not convert " <> show n <> " to an Int"
  Just n' -> Right n'
asInt (String s) = fmap fst (T.decimal s)
asInt o          = Left $ "Expected an integer but received: " <> show o


-- | Try to extract a Double from Value
--
-- >>> asScientific (Number 10.3)
-- Right 10.3
--
-- >>> asScientific (String "10.5")
-- Right 10.5
--
-- >>> asScientific (String "foo")
-- Left "Cannot convert `foo` to a number"
asScientific :: Value -> Either String S.Scientific
asScientific (Number n) = Right n
asScientific (String s) = case readMaybe (T.unpack s) of
  Nothing -> Left $ "Cannot convert `" <> T.unpack s <> "` to a number"
  Just s' -> Right s'
asScientific o = Left $ "Expected a number, but received: " <> show o


asArray :: Value -> Either String (V.Vector Value)
asArray (Array x) = Right x
asArray o         = Left $ "Expected an array, but received: " <> show o


-- | Try to extract a Text from Value
--
-- >>> asText (String "foo")
-- Right "foo"
--
-- >>> asText (Number 10.3)
-- Right "10.3"
asText :: Value -> Either String T.Text
asText (String t) = Right t
asText (Number n) = Right . T.pack $ show n
asText o          = Left $ "Expected a string, but received: " <> show o
