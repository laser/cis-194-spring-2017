module Homework.Week07.Scrabble where

import Data.Char
import Data.Monoid

newtype Score = Score Int -- replace this type with your own
  deriving (Eq, Ord, Show)

addScore :: Score -> Score -> Score
addScore (Score a) (Score b) = Score (a + b)

instance Monoid Score where
  mempty  = Score 0
  mappend = addScore

score :: Char -> Score
score cc
  | c == 'a' = (Score 1)
  | c == 'b' = (Score 3)
  | c == 'c' = (Score 3)
  | c == 'd' = (Score 2)
  | c == 'e' = (Score 1)
  | c == 'f' = (Score 4)
  | c == 'g' = (Score 2)
  | c == 'h' = (Score 4)
  | c == 'i' = (Score 1)
  | c == 'j' = (Score 8)
  | c == 'k' = (Score 5)
  | c == 'l' = (Score 1)
  | c == 'm' = (Score 3)
  | c == 'n' = (Score 1)
  | c == 'o' = (Score 1)
  | c == 'p' = (Score 3)
  | c == 'q' = (Score 10)
  | c == 'r' = (Score 1)
  | c == 's' = (Score 1)
  | c == 't' = (Score 1)
  | c == 'u' = (Score 1)
  | c == 'v' = (Score 4)
  | c == 'w' = (Score 4)
  | c == 'x' = (Score 8)
  | c == 'y' = (Score 4)
  | c == 'z' = (Score 10)
  | otherwise = (Score 0)
  where c = toLower cc

scoreString :: String -> Score
scoreString (c : []) = score c
scoreString (c : rest) = score c <> scoreString rest
