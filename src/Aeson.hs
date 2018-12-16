
module Aeson (
  asInt,
  asText,
  asDouble,
  asArray
) where


import           Data.Aeson      (Value (..))
import qualified Data.Scientific as S
import qualified Data.Text       as T
import qualified Data.Text.Read  as T
import qualified Data.Vector     as V

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
-- >>> asDouble (Number 10.3)
-- Right 10.3
--
-- >>> asDouble (String "10.5")
-- Right 10.5
--
-- >>> asDouble (String "foo")
-- Left "Expected a double, but received: foo"
asDouble :: Value -> Either String Double
asDouble (Number n) = Right $ S.toRealFloat n
asDouble (String s) =
  case T.double s of
    (Right (n, _)) -> Right n
    (Left _)       -> Left $ "Expected a double, but received: " <> T.unpack s
asDouble o          = Left $ "Expected a double, but received: " <> show o


asArray :: Value -> V.Vector Value
asArray (Array x) = x
asArray o         = error $ "Expected an array, but received: " <> show o


-- | Try to extract a Text from Value
--
-- >>> asText (String "foo")
-- "foo"
--
-- >>> asText (Number 10.3)
-- "10.3"
asText :: Value -> T.Text
asText (String t) = t
asText (Number n) = T.pack $ show n
asText o          = error $ "Expected a string, but received: " <> show o
