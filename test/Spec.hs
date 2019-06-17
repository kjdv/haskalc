import Test.Hspec
import TokenizeSpec
import ParserSpec

main :: IO ()
main = hspec $ do
  tokenizeSpec
  parserSpec
