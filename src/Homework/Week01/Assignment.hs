module Homework.Week01.Assignment where

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (xs) = reverseList' xs []

reverseList' :: [a] -> [a] -> [a]
reverseList' [] l = l
reverseList' (originalHead:originalTail) reversed =
  reverseList' originalTail (originalHead:reversed)

-- #1a
toDigits :: Integer -> [Integer]
toDigits n = reverseList (toDigitsRev n)

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = doubleEveryOther' (reverseList xs) [] 1

doubleEveryOther' :: [Integer] -> [Integer] -> Integer -> [Integer]
doubleEveryOther' [] acc _ = acc
doubleEveryOther' (x:xs) acc mutiplier = 
  let newMutiplier = (mutiplier `mod` 2) + 1
  in  doubleEveryOther' xs ((x * mutiplier):acc) newMutiplier

-- #3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) 
  | x < 10 = x + sumDigits xs
  | otherwise = sumDigits (toDigitsRev x) + sumDigits xs

-- #4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
