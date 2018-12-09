{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Fake (
  State,
  Env(..),
  newEnv,
  eval,
) where

import qualified Aeson                            as A
import           Control.Monad                    (forM)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Aeson                       (Value (..), object)
import qualified Data.ByteString.Char8            as BS
import qualified Data.HashMap.Strict              as M
import qualified Data.Scientific                  as S
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.UUID                        as UUID
import qualified Data.UUID.V1                     as UUID1
import qualified Data.Vector                      as V
import           Expr                             (Expr (..))
import           Prelude                          hiding (lines, replicate)
import           System.Random                    (StdGen, newStdGen, random,
                                                   randomR)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import System.Random (mkStdGen)
-- >>> let g = Env (mkStdGen 1) M.empty
-- >>> let exec expr = State.evalStateT (eval expr) g


type State a = StateT Env IO a

data Env = Env
  { envStdGen :: StdGen
  , envFileCache :: M.HashMap T.Text (V.Vector Value) }


newEnv :: IO Env
newEnv = do
  stdGen <- newStdGen
  pure $ Env stdGen M.empty


withStdGen :: (StdGen -> (a, StdGen)) -> State a
withStdGen f = do
  e@Env{..} <- State.get
  let
    (x, stdGen) = f envStdGen
  State.put $ e { envStdGen = stdGen }
  pure x


uuid1 :: State UUID.UUID
uuid1 = do
  uuid <- liftIO UUID1.nextUUID
  case uuid of
    (Just u) -> pure u
    Nothing  -> uuid1


-- | Generate a random int
--
-- >>> exec "randomInt(1, 2)"
-- Number 2.0
randomInt :: Expr -> Expr -> State Value
randomInt lower upper = do
  lower' <- A.asInt <$> eval lower
  upper' <- A.asInt <$> eval upper
  Number . fromIntegral <$> withStdGen (randomR (lower', upper'))


-- | Generate a random double
--
-- >>> exec "randomDouble(1.5, 3)"
-- Number 1.6166855626250152
randomDouble :: Expr -> Expr -> State Value
randomDouble lower upper = do
  lower' <- A.asDouble <$> eval lower
  upper' <- A.asDouble <$> eval upper
  Number . S.fromFloatDigits <$> withStdGen (randomR (lower', upper'))


-- | Select one random item of an array
--
-- >>> exec "oneOf(array(37, 42, 21))"
-- Number 21.0
oneOfArray :: Expr -> State Value
oneOfArray arr = do
  arr' <- A.asArray <$> eval arr
  idx <- withStdGen $ randomR (0, length arr' - 1)
  pure $ arr' V.! idx


-- | Select one random argument
--
-- >>> exec "oneOf(37, 42, 21)"
-- Number 21.0
oneOfArgs :: [Expr] -> State Value
oneOfArgs args = do
  idx <- withStdGen $ randomR (0, length args - 1)
  eval (args !! idx)


-- | Create an array with `num` items
--
-- >>> exec "replicate(randomInt(2, 4), oneOf(37, 42, 21))"
-- Array [Number 42.0,Number 42.0,Number 21.0,Number 42.0]
--
replicate :: Expr -> Expr -> State Value
replicate num expr = do
  num' <- A.asInt <$> eval num
  Array <$> V.replicateM num' (eval expr)


-- | Create an object from a list in the  [key, value [, ...]] form
--
-- >>> exec "object('x', randomInt(2, 4), oneOf('y', 'z'), 3)"
-- Object (fromList [("x",Number 4.0),("y",Number 3.0)])
-- 
objectFromArgs :: [Expr] -> State Value
objectFromArgs args = do
  let
    keyValuePairs = mkPairs (fmap eval args)
    mkPairs [] = []
    mkPairs [_] = error "Arguments to object must be a multiple of 2 (key + value pairs)"
    mkPairs (x : y : rest) = (x, y) : mkPairs rest
  pairs <- forM keyValuePairs (\(key, val) -> do
    key' <- A.asText <$> key
    val' <- val
    pure (key', val'))
  pure $ object pairs


fromFile :: Expr -> State Value
fromFile fileName = do
  fileName' <- A.asText <$> eval fileName
  e@Env{..} <- State.get
  case M.lookup fileName' envFileCache of
    (Just lines) -> pure $ Array lines
    Nothing      -> do
      contents <- liftIO $ BS.readFile (T.unpack fileName')
      let
        lines = V.fromList $ fmap (String . T.decodeUtf8) (BS.lines contents)
      State.put e { envFileCache = M.insert fileName' lines envFileCache }
      pure $ Array lines

-- | Create a value getter for an expression
--
-- >>> exec "uuid4"
-- String "0099a82c-36f7-4321-8012-daa4305fd84b"
--
-- >>> exec "array(randomInt(1, 10), randomDouble(1, 20))"
-- Array [Number 6.0,Number 14.108305934824038]
--
eval :: Expr -> State Value
eval (IntLiteral x)    = pure $ Number $ fromInteger x
eval (StringLiteral x) = pure $ String x
eval (DoubleLiteral x) = pure $ Number x
eval (FunctionCall "uuid4" []) = String . UUID.toText <$> withStdGen random
eval (FunctionCall "uuid1" []) = String . UUID.toText <$> uuid1
eval (FunctionCall "randomInt" [lower, upper]) = randomInt lower upper
eval (FunctionCall "randomDouble" [lower, upper]) = randomDouble lower upper
eval (FunctionCall "array" args) = Array . V.fromList <$> mapM eval args
eval (FunctionCall "oneOf" [arg]) = oneOfArray arg
eval (FunctionCall "oneOf" args) = oneOfArgs args
eval (FunctionCall "replicate" [num, expr]) = replicate num expr
eval (FunctionCall "object" args) = objectFromArgs args
eval (FunctionCall "fromFile" [fileName]) = fromFile fileName
eval (FunctionCall name _) = error $ "No random generator for " <> T.unpack name
