
import Test.DocTest

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/Expr.hs"
  , "app/Main.hs"
  ]
