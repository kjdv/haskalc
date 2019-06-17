module TokenizeSpec where

import Test.Hspec
import Tokenize

tokenizeSpec :: Spec
tokenizeSpec = do
  describe "tokenize" $ do
    it "recongnizes the + sign" $ do
      tokenize "+" `shouldBe` [Plus]
