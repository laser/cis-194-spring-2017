module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits n
  | n < 1 = []
  | otherwise = (toDigits (div n 10)) ++ [(mod n 10)]

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse (reallyDoubleEveryOther (reverse l))

reallyDoubleEveryOther :: [Integer] -> [Integer]
reallyDoubleEveryOther [] = []
reallyDoubleEveryOther (x:[]) = [x]
reallyDoubleEveryOther (x:y:xs) = x : y * 2 : reallyDoubleEveryOther xs

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
