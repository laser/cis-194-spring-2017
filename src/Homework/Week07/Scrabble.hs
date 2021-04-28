module Homework.Week07.Scrabble (
  score,
  scoreString,
  Score(..)
) where

import Data.Char
import Data.Monoid

newtype Score = Score Int
  deriving (Show, Eq)

instance Monoid Score where
  mempty  = Score 0
  mappend (Score a) (Score b) = Score (a + b)

score :: Char -> Score
score c
  | elem (toLower c) ['a','e','i','l','n','o','r','s','t','u'] = Score 1
  | elem (toLower c) ['d','g']             = Score 2
  | elem (toLower c) ['b','c','m','p']     = Score 3
  | elem (toLower c) ['f','h','v','w','y'] = Score 4
  | elem (toLower c) ['k']                 = Score 5
  | elem (toLower c) ['j','x']             = Score 8
  | elem (toLower c) ['q','z']             = Score 10
  | otherwise                              = Score 0

scoreString :: String -> Score
scoreString str = foldr (<>) (Score 0) (map score str)
