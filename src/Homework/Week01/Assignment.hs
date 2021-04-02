module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- #2
doubleEveryOtherFromFront :: [Integer] -> [Integer]
doubleEveryOtherFromFront (x:y:rest) = (x:2*y:doubleEveryOtherFromFront rest)
doubleEveryOtherFromFront list = list

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromFront . reverse

-- #3
sumDigits :: [Integer] -> Integer
sumDigits list = foldr (+) 0 (concatMap toDigitsRev list)

-- #4
validate :: Integer -> Bool
validate = (0 ==) . ((flip mod) 10) . sumDigits . doubleEveryOther . toDigits

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
