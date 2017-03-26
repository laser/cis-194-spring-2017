module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
-- Skips happens in three steps:
--  (1) given the length of the list as l
--  (2) for each number n from 1 -> l, generate a list of i by counting with a stride of n from n -> l
--  (3) map each i to xs[i-1]
skips :: [a] -> [[a]]
skips xs = let l = length xs in
               ((\i -> xs !! (i-1)) <$>) . (\n -> [n,n*2..l]) <$> [1..l]

-- #2
-- Local maxima happens in three steps:
--  (1) braid together the list of numbers: [1,2,3] -> [(1,1,2), (1,2,3), (2,3,3)]
--  (2) filter points in braid [(x,y,z)] where y > x && y > z
--  (3) unbraid remaining points by selecting all y in [(x,y,z)]
localMaxima :: [Integer] -> [Integer]
localMaxima xs = (\(_,y,_) -> y) <$> filter (\(x,y,z) -> y > x && y > z) (zip3 (drop 1 xs) xs (head xs:xs))

-- #3
histogram :: [Integer] -> String
histogram = undefined
