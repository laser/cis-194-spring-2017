module Homework.Week07.Scrabble where

newtype Score = Score Int
  deriving (Show, Eq)

instance Monoid Score where
  mempty = undefined
  mappend = undefined

score :: Char -> Score
score 'a' = Score 1
score 'b' = Score 3
score 'c' = Score 3
score 'd' = Score 2
score 'e' = Score 1
score 'f' = Score 4
score 'g' = Score 2
score 'h' = Score 4
score 'i' = Score 1
score 'j' = Score 8
score 'k' = Score 5
score 'l' = Score 1
score 'm' = Score 3
score 'n' = Score 1
score 'o' = Score 1
score 'p' = Score 3
score 'q' = Score 10
score 'r' = Score 1
score 's' = Score 1
score 't' = Score 1
score 'u' = Score 1
score 'v' = Score 4
score 'w' = Score 4
score 'x' = Score 8
score 'y' = Score 4
score 'z' = Score 10
score _ = Score 0

scoreString :: String -> Score
scoreString "" = Score 0
scoreString (x:xs) = mappend (score x) (scoreString xs)
