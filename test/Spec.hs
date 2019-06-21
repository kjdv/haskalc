import Test.Hspec
import TokenizerSpec
import ParserSpec
import EvaluatorSpec

main :: IO ()
main = hspec $ do
  tokenizerSpec
  parserSpec
  evaluatorSpec
