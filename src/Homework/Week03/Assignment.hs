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
histogramNumbers = [0 .. 9]

histogram :: [Integer] -> String
histogram xs =
  let counts = histogramCounts xs
      maxCount = maximum counts
      numberLine = intercalate "" . map show $ histogramNumbers
      axisLine = replicate (length histogramNumbers) '='
      columns = reverse . transpose . map (histogramRow maxCount) $ counts
  in intercalate "\n" . reverse $ ("" : numberLine : axisLine : columns)

histogramCounts :: [Integer] -> [Int]
histogramCounts xs =
  let grouped = group . sort $ xs
  in histogramCounts' grouped histogramNumbers []

histogramCounts' :: [[Integer]] -> [Integer] -> [Int] -> [Int]
histogramCounts' _ [] acc = reverse acc
histogramCounts' (x@(xn:_):xs) (n:ns) acc
  | xn == n = histogramCounts' xs ns (length x : acc)
histogramCounts' xs (n:ns) acc = histogramCounts' xs ns (0 : acc)

histogramRow :: Int -> Int -> String
histogramRow maxCount n = replicate (maxCount - n) ' ' ++ replicate n '*'
