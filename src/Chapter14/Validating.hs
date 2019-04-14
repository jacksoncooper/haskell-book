-- Chapter Exercises, Page 569

module Chapter14.Validating where

import Test.Hspec

import Chapter08.Exercises (digitToWord, digits, wordNumber)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "Returns 'zero' for 0." $ do
      digitToWord 0 `shouldBe` "zero"
    it "Returns 'one' for 1." $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "Returns [1] for 1." $ do
      digits 1 `shouldBe` [1]
    it "Returns [1, 0, 0] for 100." $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "Returns 'one-zero-zero' for 100." $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "Returns 'nine-zero-zero-one' for 9001." $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"