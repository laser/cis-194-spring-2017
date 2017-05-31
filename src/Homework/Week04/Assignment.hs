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
import Data.List
import Data.Char
import Data.Maybe

-- #1
-- Only one function inhabits the type.
ex1 :: a -> b -> b
ex1 _ b = b

-- #2
-- Two functions inhabit the type - it could return the
-- first or second paramater.
ex2 :: a -> a -> a
ex2 x _ = x

-- #3
-- Only one function inhabits the type.
ex3 :: Int -> a -> a
ex3 _ a = a

-- #4
-- There could be 4 functions inhabiting this type, since
-- True could also return y and vice versa.
ex4 :: Bool -> a -> a -> a
ex4 True x _ = x
ex4 False _ y = y

-- #5
-- There could be 4 functions inhabiting this type, since
-- True could also return True and vice versa.
ex5 :: Bool -> Bool
ex5 True = False
ex5 False = True

-- #6
-- This needs some data to run the method on.
ex6 :: (a -> a) -> a
ex6 = error "impossible!"

-- #7
-- This just calls the first function.
-- Only one function inhabits the type.
ex7 :: (a -> a) -> a -> a
ex7 f = f

-- #8
-- If a is [], that's fine.
-- Only one function inhabits the type.
ex8 :: [a] -> [a]
ex8 a = a

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 = map

-- #10
-- If a is Nothing, there's nothing to return.
ex10 :: Maybe a -> a
ex10 = error "impossible!"

-- #11
-- Only one function inhabits the type.
ex11 :: a -> Maybe a
ex11 = Just

-- #12
-- Only one function inhabits the type.
ex12 :: Maybe a -> Maybe a
ex12 a = a

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST sorter x (Node left y right)
  | comparison == LT = Node (insertBST sorter x left) y right
  | comparison == GT || comparison == EQ = Node left y (insertBST sorter x right)
  where comparison = sorter x y

-- #14
-- listToMaybe returns the fisrt element in a maybe if there is one,
-- or Nothing if there isn't. fromMaybe takes a default,
-- so default to a non-capital letter if the string was empty.
allCaps :: [String] -> Bool
allCaps = all (isUpper . fromMaybe 'a' . listToMaybe)

-- #15
-- This solution comes right from the haskell docs.
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = dropWhileEnd isSpace

-- #16
-- null checks for empty lists, so filter those out, then
-- safely use head.
firstLetters :: [String] -> [Char]
firstLetters = map head . filter (not . null)

-- #17
asList :: [String] -> String
asList xs =
  let filtered = filter (\string -> length string > 0) xs
  in "[" ++ intercalate "," filtered ++ "]"
