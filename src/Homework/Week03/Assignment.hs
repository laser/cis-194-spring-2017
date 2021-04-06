module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram,
  histoHelper,
  incSecondHead
) where

-- #1 --
skips :: [a] -> [[a]]
skips list = map (skip list) [1..length list]

-- I think this could be written more resursively? You could write a helper
-- that only performs the operation on the first part of the list, and then
-- resursively define that?
skip [] n = []
skip list n = if length list >= n then ((head . drop (n-1)) list):(skip (drop n list) n) else []

-- #2 --
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:rest) = if (x < y && y > z) then (y:localMaxima (z:rest)) else localMaxima (y:z:rest)
localMaxima list         = []


-- #3
histogram :: [Integer] -> String
histogram = undefined
-- histogram list = foldr ({-- function --}) (replicate 10 0) list

-- 0 |
-- 1 | ***
-- 2 | *

-- Takes a list of Integers and returns a list of Integers with length 10 of
-- the form [num of 0's, ..., num of 9's] in the initial list
incidences :: [Integer] -> [Integer]
incidences list = foldr incHelper (replicate 10 0) list

incHelper :: Integer -> [Integer] -> [Integer]
incHelper i occs = joinPair $ incSecondHead (splitAt (fromInteger i) occs)

-- Takes a pair of Integer lists and increments the head of the second --
incSecondHead :: ([Integer],[Integer]) -> ([Integer],[Integer])
incSecondHead (x,(yh:yt)) = (x,(yh+1:yt))

joinPair :: ([a],[a]) -> [a]
joinPair (x,y) = x ++ y
