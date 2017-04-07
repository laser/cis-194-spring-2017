module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import Prelude

skips' :: [a] -> Int -> [a]
skips' _ 0 = []
skips' list n
  | null list = []
  | length list == n = [last list]
  | length list < n = []
  | otherwise = do
    let split = splitAt n list
    last (fst split) : skips' (snd split) n

-- #1
skips :: [a] -> [[a]]
skips list@(x : xs)
  | null xs = [[x]]
  | otherwise = map (skips' list) [1..length list]
skips _ = []

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima = undefined

-- #3
histogram :: [Integer] -> String
histogram = undefined
