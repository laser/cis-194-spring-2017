module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits n
  | n < 1 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
  | length xs `mod` 2 == 0 = x : doubleEveryOther xs
doubleEveryOther (x:(y:s)) = x * 2 : y : doubleEveryOther s

-- #3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- #4
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n source dest spare = hanoi (n-1) source spare dest ++ [(source, dest)] ++ hanoi (n -1) spare dest source

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
