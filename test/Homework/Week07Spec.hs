module Homework.Week07Spec (
  main,
  spec
) where

import Test.Hspec
import Test.QuickCheck

import Homework.Week07.Sized
import Homework.Week07.JoinList

import Data.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tag" $ do
    it "gets the annotation at the root of a JoinList" $ do
      pending
      tag (Append (Sum 5) (Single (Sum 3) 'a') (Single (Sum 2) 'b')) `shouldBe` (Sum 5)
      tag (Single (Sum 10) 'a') `shouldBe` (Sum 10)

  describe "+++" $ do
    it "appends two JoinList structures together" $ do
      pending
      let a = (Single (Sum 3) 'a')
      let b = (Single (Sum 2) 'b')
      (+++) a b `shouldBe` (Append (Sum 5) a b)

  describe "exercise 2" $ do
    let jl = (Append (Size 3) (Append (Size 2) (Single (Size 1) 's') (Single (Size 1) 'u')) (Single (Size 1) 'p'))

    describe "indexJ" $ do
      it "finds a letter at the specified index" $ do
        pending
        (indexJ 0 jl) `shouldBe` (Just 's')
        (indexJ 1 jl) `shouldBe` (Just 'u')
        (indexJ 2 jl) `shouldBe` (Just 'p')

    describe "dropJ" $ do
      it "returns the list when dropping 0" $ do
        pending
        (dropJ 0 jl) `shouldBe` jl
      it "drops the first element from a JoinList" $ do
        pending
        (dropJ 1 jl) `shouldBe` Append (Size 2) (Single (Size 1) 'u') (Single (Size 1) 'p')
      it "drops the first n elements from a JoinList" $ do
        pending
        (dropJ 2 jl) `shouldBe` Single (Size 1) 'p'

    describe "takeJ" $ do
      it "returns the list when taking more than the list" $ do
        pending
        (takeJ 4 jl) `shouldBe` jl
      it "takes the first element from a JoinList" $ do
        pending
        (takeJ 1 jl) `shouldBe` Single (Size 1) 's'
      it "takes the first n elements from a JoinList" $ do
        pending
        (takeJ 2 jl) `shouldBe` Append (Size 2) (Single (Size 1) 's') (Single (Size 1) 'u')

  describe "scoreLine" $ do
    it "does what you tell it to do" $ do
      pending
