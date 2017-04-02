module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import Data.List

-- #1
skips :: [a] -> [[a]]
skips xs = map ((flip s) xs) [1..length xs]
  where s n = map snd . filter fst . zip (cycle (replicate (n - 1) False ++ [True]))

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima = map m . filter mx . ts
  where m (_, x, _) = x
        mx (x, y, z)
          | x < y && y > z = True
          | otherwise = False
        ts (x : xs@(y : z : _)) = (x, y, z) : ts xs
        ts _ = []

-- #3
histogram :: [Integer] -> String
histogram xs = intercalate "\n" $ reverse(hr) ++ ["==========\n0123456789\n"]
  where hr = map ((flip ss) cs) [1..maximum cs]
        cs = map ((flip c) xs) [0..9]
        c n = length . filter (== n)
        ss n = map (s n)
        s n x = if x >= n then '*' else ' '
