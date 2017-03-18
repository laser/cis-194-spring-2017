module Homework.Week01.Assignment where

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList(xs) ++ [x]

-- #1a
toDigits :: Integer -> [Integer]
toDigits n = reverseList (toDigitsRev n)

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = undefined

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = undefined

-- #4
validate :: Integer -> Bool
validate = undefined

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
