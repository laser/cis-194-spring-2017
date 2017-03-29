module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n | n < 0 = []
toDigits n = toDigits(n `div` 10) ++ [n `mod` 10]

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n | n < 0 = []
toDigitsRev n = n `mod` 10 :  toDigitsRev(n `div` 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther a | even (length a) = [head a * 2] ++ doubleEveryOther(tail a)
doubleEveryOther a = [head a] ++ doubleEveryOther(tail a)


-- #3
flatDigits :: [Integer] -> [Integer]
flatDigits = concatMap toDigits

sumDigits :: [Integer] -> Integer
sumDigits = sum . flatDigits

-- #4
isDivZero :: Integer -> Bool
isDivZero n =  0 == n `mod` 10

validate :: Integer -> Bool
validate = isDivZero . sumDigits . doubleEveryOther . toDigits

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
