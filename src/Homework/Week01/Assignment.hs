module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0 = (n `mod` 10):toDigitsRev (n `div` 10)
  | otherwise = []

-- #2
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' = zipWith (*) (cycle [1,2])

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOther' . reverse

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

-- #4
isDivisibleByTen :: Integer -> Bool
isDivisibleByTen = (==0) . (`mod`10)

validate :: Integer -> Bool
validate = isDivisibleByTen . sumDigits . doubleEveryOther . toDigits

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
