{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main
import           Data.Aeson     (Value (..))
import           Expr           (Expr (..))
import           Fake           (eval, runFakeT)


exec :: Expr -> IO Value
exec expr = runFakeT (Just 1) (eval expr)


main :: IO ()
main = defaultMain
  [ bgroup "ids" [ bench "ulid" $ nfIO (exec "ulid")
                 , bench "uuid1" $ nfIO (exec "uuid1")
                 , bench "uuid4" $ nfIO (exec "uuid4") ]
  , bgroup "numbers" [ bench "randomInt-0-100"  $ nfIO (exec "randomInt(0, 100)" )
                     , bench "randomInt-0-1000" $ nfIO (exec "randomInt(0, 1000)" ) ]
  ]
