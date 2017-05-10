module Homework.Week07Spec (
  main,
  spec
) where

import Test.Hspec
import Test.QuickCheck

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

  describe "indexJ" $ do
    it "does what you tell it to do" $ do
      pending

  describe "dropJ" $ do
    it "does what you tell it to do" $ do
      pending

  describe "takeJ" $ do
    it "does what you tell it to do" $ do
      pending

  describe "scoreLine" $ do
    it "does what you tell it to do" $ do
      pending