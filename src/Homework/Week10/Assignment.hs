module Homework.Week10.Assignment where

import Homework.Week10.Support (Tree(..), labelTree)

import Test.QuickCheck
import Control.Monad

-- Exercise 1

treeOf :: Arbitrary a => Int -> Gen (Tree a)
treeOf n | n <= 1 = Leaf <$> arbitrary
treeOf n = 
  let nodesOnLeft = n `div` 2
  in  liftM2 Node (treeOf nodesOnLeft) (treeOf (n - nodesOnLeft))

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized $ \size -> do
    len <- choose (0, size)
    treeOf len

-- Exercise 2

size :: Tree a -> Int
size (Leaf _) = 1
size (Node tree1 tree2) = size tree1 + size tree2

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Node tree1 tree2) = toList tree1 ++ toList tree2

-- Exercise 3

-- The length of the list produced by toList is the size of the given tree.
prop_lengthToList :: Tree Integer -> Bool
prop_lengthToList tree = (length . toList $ tree) == size tree

-- labelTree does not change the size of the tree.
prop_sizeLabelTree :: Tree Integer -> Bool
prop_sizeLabelTree tree = size tree == size (labelTree tree)

-- For every tree t, toList (labelTree t) is the expected list.
-- Hint: [0..n] denotes the list of numbers from 0 to n, inclusively.
prop_labelTree :: Tree Integer -> Bool
prop_labelTree tree = (toList . labelTree $ tree) == [0..(toInteger . size $ tree) - 1]

-- Applying labelTree to a list twice does yield the same list as applying it once.
prop_labelTreeIdempotent :: Tree Integer -> Bool
prop_labelTreeIdempotent tree = (toList . labelTree $ tree) == (toList . labelTree . labelTree $ tree)
