
import           System.FilePath.Glob (glob)
import           Test.DocTest

main :: IO ()
main = do
  sources <- glob "src/**/*.hs"
  doctest ("app/Main.hs" : sources)
