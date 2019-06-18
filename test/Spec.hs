import Test.Hspec
import TokenizerSpec
import ParserSpec

main :: IO ()
main = hspec $ do
  tokenizerSpec
  parserSpec
