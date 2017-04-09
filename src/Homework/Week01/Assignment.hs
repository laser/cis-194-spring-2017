module Homework.Week01.Assignment where
i :: Int
i = 78

-- #1a
-- This is properly tail-recursive, but does it matter with lazy list-construction?
toDigits' :: Integer -> [Integer] -> [Integer]
toDigits' 0 acc = acc
toDigits' i acc = toDigits' (div i 10) (mod i 10 : acc)

toDigits :: Integer -> [Integer]
toDigits i = toDigits' i []

-- #1b
-- This is not properly tail-recursive, but the lazy list's head never depends
-- on the recursive call, so I think it's actually fine.
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev i = mod i 10 : toDigitsRev (div i 10)

-- #2
-- Double every other number starting from LEFT.
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [n] = [2 * n]
doubleEveryOther' (m : n : rest) = m : (2 * n) : doubleEveryOther' rest

-- A lot of extra list-traversal here considering that all CC numbers have
-- sixteen digits.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther digits = reverse (doubleEveryOther' (reverse digits))

-- #3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n : rest) | n > 9 = sumDigits ((sumDigits (toDigits n)) : rest)
sumDigits (n : rest) = n + sumDigits rest

-- #4
-- This works but performs extra reversal since the order of the list
-- given to sumDigits doesn't matter.
validate :: Integer -> Bool
validate i = sumDigits (doubleEveryOther (toDigits i)) `mod` 10 == 0

-- This performs correct doubling without needing extra reversal because
-- we can 'reverse' for free when constructing the initial digits list.
validate' :: Integer -> Bool
validate' i = sumDigits (doubleEveryOther' (toDigitsRev i)) `mod` 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

-- Use an accumulator so we can build the result in the correct order and with
-- the correct (spliced) structure.
hanoi' :: Integer -> Peg -> Peg -> Peg -> [Move] -> [Move]
hanoi' 0 a b c acc = acc
hanoi' n a b c acc = hanoi' (n - 1) a c b ((a, b) : (hanoi' (n-1) c b a acc))

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c = hanoi' n a b c []
