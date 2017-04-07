module Homework.Week04.Assignment (
  ex1,
  ex2,
  ex3,
  ex4,
  ex5,
  ex6,
  ex7,
  ex8,
  ex9,
  ex10,
  ex11,
  ex12,
  insertBST,
  allCaps,
  dropTrailingWhitespace,
  firstLetters,
  asList,
  BST(..)
) where

import Homework.Week04.BST

import Data.Maybe
import Data.List

-- #1
ex1 :: a -> b -> b
ex1 _ x = x

-- #2
ex2 :: a -> a -> a
ex2 x _ = x

-- #3
ex3 :: Int -> a -> a
ex3 _ x = x

-- #4
ex4 :: Bool -> a -> a -> a
ex4 _ _ x = x

-- #5
ex5 :: Bool -> Bool
ex5 x = x

-- #6
ex6 :: (a -> a) -> a
ex6 = undefined

-- #7
ex7 :: (a -> a) -> a -> a
ex7 f y = f y

-- #8
ex8 :: [a] -> [a]
ex8 xs = xs

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 f xs = map f xs

-- #10
ex10 :: Maybe a -> a
ex10 = error "impossible"

-- #11
ex11 :: a -> Maybe a
ex11 x = Just x

-- #12
ex12 :: Maybe a -> Maybe a
ex12 x = x

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ message Leaf = Node Leaf message Leaf
insertBST cmp x (Node left y right)
        | cmp x y == LT = Node (insertBST cmp x left) y right
        | cmp x y == GT = Node left y (insertBST cmp x right)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

-- #14
allCaps :: [String] -> Bool
allCaps [] = True
allCaps xs = all cap $ map safeHead xs
  where cap (Just x) = elem x ['A'..'Z']
        cap _ = False

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = foldr d ""
  where d a b
          | a == ' ' && isNothing (safeHead b) = b
          | otherwise = a : b

-- #16
firstLetters :: [String] -> [Char]
firstLetters xs = map fromJust $ filter isJust $ map safeHead xs

-- #17
asList :: [String] -> String
asList xs = "[" ++ (intercalate "," xs) ++ "]"
