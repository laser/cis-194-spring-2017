module Homework.Week07.Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty  = Score 0
  mappend (Score x) (Score y) = Score (x + y)

score :: Char -> Score
score c
  | lowerC `elem` "aeilnorstu" = Score 1
  | lowerC `elem` "dg" = Score 2
  | lowerC `elem` "bcmp" = Score 3
  | lowerC `elem` "fhvwy" = Score 4
  | lowerC `elem` "k" = Score 5
  | lowerC `elem` "jx" = Score 8
  | lowerC `elem` "qz" = Score 10
  | otherwise = Score 0
  where lowerC = toLower c

scoreString :: String -> Score
scoreString = mconcat . map score
