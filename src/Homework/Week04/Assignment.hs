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
import Data.Char
import Data.List
import Data.Maybe

-- #1
-- Return type is fully determined by second argument.
ex1 :: a -> b -> b
ex1 _ b = b

-- #2
-- Return type is determined by either first or second argument, which must of the same type.
ex2 :: a -> a -> a
ex2 a _ = a

-- #3
-- Return type is dertermined by second argument. First must be an integer.
ex3 :: Int -> a -> a
ex3 _ a = a

-- #4
-- Return type is determined by second and third arguments, which must have same type.
-- First argument must be boolean.
ex4 :: Bool -> a -> a -> a
ex4 _ a _ = a

-- #5
-- Takes a boolean and returns one.
ex5 :: Bool -> Bool
ex5 x = not x

-- #6
-- Cannot deduce domain of the function to call it or otherwise produce a value of that type.
ex6 :: (a -> a) -> a
ex6 = error "impossible"

-- #7
-- First argument is a function which, when applied to the second returns the overall return type
-- which is the same as that of the second argument.
ex7 :: (a -> a) -> a -> a
ex7 f x = (f x)

-- #8
-- Takes a list of some type and returns a list of the same type.
ex8 :: [a] -> [a]
ex8 x = x

-- #9
-- Takes a function from the type of which the second argument is a list to the type of which the return
-- value is a list.
ex9 :: (a -> b) -> [a] -> [b]
ex9 f l = map f l

-- #10
-- Cannot produce an a from Nothing
ex10 :: Maybe a -> a
ex10 = error "impossible"

-- #11
-- Make whatever it receives into a Maybe. Could just always return Nothing, too.
ex11 :: a -> Maybe a
ex11 x = Just x

-- #12
-- Like 11, could just always return nothing, but let's be clear and explicit about what's going on.
ex12 :: Maybe a -> Maybe a
ex12 Nothing = Nothing
ex12 (Just x) = Just x

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBst comp x (Node left xt right) =
  case (comp x xt) of
    LT -> Node (insertBST comp x left) xt right
    EQ -> Node (insertBST comp x left) xt right
    GT -> Node left xt (insertBST comp x right)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

-- #14
allCaps :: [String] -> Bool
allCaps words = all (\ word -> (maybe False isUpper (safeHead word))) words

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace s = dropWhileEnd isSpace s

-- #16
firstLetters :: [String] -> [Char]
firstLetters words = mapMaybe safeHead words

-- #17
asList :: [String] -> String
asList words = "[" ++ (intercalate "," (filter (isJust . safeHead) words)) ++ "]"
