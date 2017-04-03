module Homework.Week03.Assignment
  ( skips
  , localMaxima
  , histogram
  ) where

-- #1
skips :: [a] -> [[a]]
skips xs = map (skipsN [] xs) [1 .. (length xs)]

skipsN :: [a] -> [a] -> Int -> [a]
skipsN acc xs n
  | length xs < n = reverse acc
  | otherwise =
    let nth:_ = (reverse . take n $ xs)
        rest = drop n xs
    in skipsN (nth : acc) rest n

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima = undefined

-- #3
histogram :: [Integer] -> String
histogram = undefined
