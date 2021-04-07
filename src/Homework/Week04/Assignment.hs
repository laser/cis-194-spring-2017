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

import Data.Char
import Data.List

import Homework.Week04.BST

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

-- #1
ex1 :: a -> b -> b
ex1 a b = b

-- #2
ex2 :: a -> a -> a
ex2 a1 a2 = a1 -- or a2

-- #3
ex3 :: Int -> a -> a
ex3 _ a = a

-- #4
ex4 :: Bool -> a -> a -> a
ex4 _ a1 a2 = a1 -- there are a few implementations here
-- Impl 1: (always return the first a)
-- _    a1 a2 = a1
-- Impl 2: (always returns the second a)
-- _    a1 a2 = a2
-- Impl 3: (True passed in returns the first a, False the second
-- True  a1 a2 = a1
-- False a1 a2 = a2
-- Impl 3: (True passed in returns the second a, False the first
-- True  a1 a2 = a2
-- False a1 a2 = a1

-- #5
ex5 :: Bool -> Bool
ex5 = undefined
-- Impl 1:
-- _ -> True
-- Impl 2:
-- _ -> False
-- Impl 3: (Boolean Identity Function)
-- True -> True
-- False -> False
-- Impl 4: (Boolean NOT Function)
-- True -> False
-- False -> True

-- #6
ex6 :: (a -> a) -> a
ex6 f = undefined
-- There is no such function, for one cannot provide an output value
-- for a function that has no way of being supplied an argument

-- #7
ex7 :: (a -> a) -> a -> a
ex7 f a = f a
-- So either apply f or don't
-- Another Impl (don't apply f)
-- ex7 _ a = a

-- #8
ex8 :: [a] -> [a]
ex8 [] = []
ex8 (a:as) = (a:as)
-- There are lots of these, but I think all of them can be regarded function from
-- a list to a permutation of a subsequence of the list

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 = undefined
-- These will all have the form of (do the mapping) then perform some kind of
-- operation that would quality as an ex8 from above

-- #10
ex10 :: Maybe a -> a
ex10 = undefined
-- This doesn't appear definable, as there is no way
-- to select an item for Nothing to map to

-- #11
ex11 :: a -> Maybe a
ex11 a = Just a
-- 2 implementations. Eitehr Just a or Nothing
-- ex11 a = Just a
-- ex11 _ = Nothing

-- #12
ex12 :: Maybe a -> Maybe a
-- there are two implementations
-- either the Nothing constant function
ex12 _ = Nothing
-- or the identity function
-- ex12 Just a  = Just a
-- ex12 Nothing = Nothing
-- Nothing must map to nothing, and because we know nothing about the type
-- a, Just a -> Nothing or Just a

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST o x (Node left y right) =
  if o x y == LT
    then (Node (insertBST o x left) y right)
    else (Node left y (insertBST o y right))

-- #14
allCaps :: [String] -> Bool
allCaps = all ((maybe False isUpper) . safeHead)

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . (dropWhile isSpace) . reverse

-- #16
firstLetters :: [String] -> [Char]
firstLetters = (map ((maybe 'x' id) . safeHead)) . (filter (not . null))

-- #17
asList :: [String] -> String
asList list = "[" ++ (intercalate "," (filter (not . null) list)) ++ "]"
