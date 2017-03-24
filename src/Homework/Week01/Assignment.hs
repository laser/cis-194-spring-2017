module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0  = n `mod` 10: toDigitsRev (n `div` 10)
  | otherwise = [] 

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse(otherFunc(reverse(n)))

otherFunc :: [Integer] -> [Integer]
otherFunc (x:y:zs) = x :(2* y ) : otherFunc(zs) 
otherFunc (y:zs) =  y : otherFunc(zs)
otherFunc (zs) = []


-- #3
sumDigits :: [Integer] -> Integer
sumDigits (y : zs) = otherFunction(toDigits(y)) + sumDigits(zs)
sumDigits []  = 0

otherFunction :: [Integer] -> Integer
otherFunction (y:zs) = y + otherFunction(zs)
otherFunction [] = 0

-- #4
validate :: Integer -> Bool
validate n 
  | sumDigits(doubleEveryOther(toDigits(n))) `mod` 10 == 0 = True
  | otherwise = False

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
