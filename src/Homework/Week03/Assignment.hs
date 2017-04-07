module Homework.Week03.Assignment
  ( skips
  , localMaxima
  , histogram
  ) where

import Data.List

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
localMaxima xs = localMaxima' xs []

localMaxima' :: [Integer] -> [Integer] -> [Integer]
localMaxima' xs acc
  | length xs < 3 = reverse acc
localMaxima' (before:x:after:xs) acc
  | before < x && x > after = localMaxima' (x : after : xs) (x : acc)
  | otherwise = localMaxima' (x : after : xs) acc

-- #3
histogram :: [Integer] -> String
histogram xs = histogram_printer (Data.List.sort xs) []

histogram_printer :: [Integer] -> [String] -> String
histogram_printer xs graph = undefined
