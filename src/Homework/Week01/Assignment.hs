module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits n | n <= 0    = []
           | otherwise = toDigits(div n 10) ++ [mod n 10]

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- #2
thing :: [Integer] -> [Integer]
thing (x:y:zs) = [x, y * 2] ++ thing zs
thing n = n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . thing . reverse

-- #3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
-- sumDigits (x:xs) = sum (toDigits x) + sumDigits xs
sumDigits xs = foldr ((+) . sum . toDigits) 0 xs

-- #4
validate :: Integer -> Bool
validate n = mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c | n == 0     = []
              | otherwise  = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined