-- Chapter Exercises, Page 573

module Chapter14.HangmanTesting where

import Test.Hspec
import Test.QuickCheck

import Chapter13.Hangman (Puzzle(Puzzle), fillInCharacter, freshPuzzle, handleGuess)

main :: IO ()
main = hspec $ do
  let puzzle0 = freshPuzzle "apple"
  let puzzle1 = Puzzle "apple" (Just 'a' : replicate 4 Nothing) "a"
  let puzzle2 = Puzzle "apple" (Just 'a' : replicate 4 Nothing) "aa"
  let puzzle3 = Puzzle "apple" (Just 'a' : replicate 4 Nothing) "saa"

  describe "freshPuzzle" $ do
    it "Create a fresh puzzle with the word \"apple\"." $ do
      freshPuzzle "apple" `shouldBe` Puzzle "apple" (replicate 5 Nothing) []

  describe "fillInCharacter" $ do
    it ("Guess the letter 'a' in puzzle: " ++ show puzzle0) $ do
      fillInCharacter puzzle0 'a' `shouldBe` puzzle1
    it ("Guess the letter 'a' again in puzzle: " ++ show puzzle1) $ do
      fillInCharacter puzzle1 'a' `shouldBe` puzzle2
    it ("Guess the letter 's' in puzzle: " ++ show puzzle2) $ do
      fillInCharacter puzzle2 's' `shouldBe` puzzle3
    
  -- This is a bit clunky. Not sure how to test console output. Otherwise
  -- handleGuess is just a redundant wrapper around fillInCharacter.

  -- describe "handleGuess" $ do
  --   it ("Guess the letter 'a' in the puzzle: " ++ show puzzle0) $ do
  --     puzzle1' <- handleGuess puzzle0 'a'
  --     puzzle1' `shouldBe` puzzle1
  -- ...