import Test.Hspec
import TokenizerSpec
import ParserSpec
import EvaluatorSpec
import ProgramSpec

main :: IO ()
main = hspec $ do
  tokenizerSpec
  parserSpec
  evaluatorSpec
  programSpec
