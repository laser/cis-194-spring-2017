module Homework.Week11Spec (
  main,
  spec
) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.Random
import Homework.Week11.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "battle" $ do
    it "subtracts two units from the battle" $ do
      pending
      newField <- evalRandIO (battle (Battlefield 3 2))
      attackers newField + defenders newField `shouldBe` 3

    it "subtracts one unit from a small battle" $ do
      pending
      newField <- evalRandIO (battle (Battlefield 2 1))
      attackers newField + defenders newField `shouldBe` 2

  describe "invade" $ 
    it "produces a winner" $ do
      pending
      newField <- evalRandIO (invade (Battlefield 10 10))
      newField `shouldSatisfy` \field ->
        case field of 
          Battlefield _ 0 -> True
          Battlefield 1 _ -> True
          _ -> False

  describe "successProb" $ do
    it "finds a low probability of success" $ do
      pending
      newField <- evalRandIO (successProb (Battlefield 2 20))
      newField `shouldSatisfy` (0.1 >)

    it "finds a high probability of success" $ do
      pending
      newField <- evalRandIO (successProb (Battlefield 20 2))
      newField `shouldSatisfy` (0.9 <)
