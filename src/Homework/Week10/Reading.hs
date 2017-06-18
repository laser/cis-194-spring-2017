module Homework.Week10.Reading where

import Test.QuickCheck
import Data.List (sort)
import Control.Monad (replicateM)

merge :: Ord a => [a] -> [a] -> [a]
merge a@(x:xs) b@(y:ys)
  | x < y = x : merge xs b
  | otherwise = y : merge a ys
merge [] ys = ys
merge xs [] = xs

prop_numElements ::  [Integer] -> [Integer] -> Bool
prop_numElements xs ys = length xs + length ys == length (merge xs ys)

prop_sorted :: OrderedList Integer -> OrderedList Integer -> Bool
prop_sorted (Ordered xs) (Ordered ys) = merge xs ys == sort (xs ++ ys)

data List a = Empty | Entry a (List a)

toList :: List a -> [a]
toList Empty = []
toList (Entry a as) = a : toList as

fromList :: [a] -> List a
fromList [] = Empty
fromList (x:xs) = Entry x $ fromList xs

instance Show a => Show (List a) where
  show = show . toList

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

-- sized :: (Int -> Gen a) -> Gen a

getSize :: Gen Int
getSize = sized return

genList :: Arbitrary a => Gen (List a)
genList = do
  len <- choose (0, 10)
  vals <- replicateM len arbitrary
  return $ fromList vals

genList' :: Arbitrary a => Gen (List a)
genList' = do
  stopNow <- arbitrary -- randomly select True or False
  if stopNow           -- if True, return Empty
    then return Empty
    else do            -- otherwise, return arbitrary value
      x <- arbitrary   -- attached to another generated list
      xs <- genList'
      return (Entry x xs)

genList'' :: Arbitrary a => Gen (List a)
genList'' = do
  size <- getSize
  len <- choose (0, size)
  vals <- replicateM len arbitrary
  return $ fromList vals

-- frequency :: [(Int, Gen a)] -> Gen a

genList''' :: Arbitrary a => Gen (List a)
genList''' = do
  size <- getSize
  frequency [ (1, return Empty)
            , (size, do x <- arbitrary
                        xs <- resize (size - 1) genList
                        return $ Entry x xs) ]

genList'''' :: Arbitrary a => Gen (List a)
genList'''' = sized $ \size -> do
  vals <- replicateM size arbitrary
  return $ fromList vals
