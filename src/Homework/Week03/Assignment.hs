module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import Data.List (group, sort, transpose)

-- #1
-- Skips happens in these steps given a list:
--  (1) given the length of the list as l
--  (2) map a list of numbers n from 1 -> l:
--    (2a) generate a list of i by counting with a stride of n from n -> l, then
--    (2b) map each i to xs[i-1]
skips :: [a] -> [[a]]
skips xs = let l = length xs in
               ((\i -> xs !! (i-1)) <$>) . (\n -> [n,n*2..l]) <$> [1..l]

-- #2
-- Local maxima happens in these steps given a list of numbers:
--  (1) braid together the list of numbers: [1,2,3] -> [(1,1,2), (1,2,3), (2,3,3)]
--  (2) filter points in braid [(x,y,z)] where y > x && y > z
--  (3) unbraid remaining points by selecting all y in [(x,y,z)]
localMaxima :: [Integer] -> [Integer]
localMaxima xs = (\(_,y,_) -> y) <$> filter (\(x,y,z) -> y > x && y > z) (zip3 (drop 1 xs) xs (head xs:xs))

-- #3
-- Histogram happens in these steps given a list of numbers:
--  (1) collect the given numbers by their occurrence [1,1,1,2,2,3] -> [[1,2,3], [1,2], [1]]
--  (2) convert collected lines to lines for the graph [[1,2,3],[1,2],[1]] -> [" *        ", " **       ", " ***       "]
--  (3) join coverted lines with '\n' and append the bottom lines "=========\n0123456789"
histogram :: [Integer] -> String
histogram xs = (++"==========\n0123456789\n") . unlines $ (\ys -> char ys <$> [0..9]) <$> (reverse . transpose . group . sort) xs
  where char :: [Integer] -> Integer -> Char
        char ys n = if n `elem` ys then '*' else ' '
