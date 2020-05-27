-- Exercises, Page 542

import Test.Hspec
import Test.QuickCheck

-- import Chapter08.Exercises (dividedBy, multiply)

main :: IO ()
main = hspec $ do
  describe "addition" $ do
    it "1 + 1 is greater than 1." $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4." $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is always greater than x." $ do
      property $ \x -> x + 1 > (x :: Int)
  
  describe "dividedBy" $ do
    it "15 divided by 3 is 5." $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2." $ do
      dividedBy 22 5 `shouldBe` (4, 2)

  describe "multiply" $ do
    it "0 multiplied by 1 is 0." $ do
      multiply 0 1 `shouldBe` 0
    it "1 multiplied by 0 is 0." $ do
      multiply 1 0 `shouldBe` 0
    it "1 multiplied by 5 is 5." $ do
      multiply 1 5 `shouldBe` 5
    it "5 multiplied by 1 is 5." $ do
      multiply 5 1 `shouldBe` 5