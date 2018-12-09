
import Test.DocTest

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/Expr.hs"
  , "src/Aeson.hs"
  , "app/Main.hs"
  ]
