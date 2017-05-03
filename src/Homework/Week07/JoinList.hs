module Homework.Week07.JoinList where

import Homework.Week07.Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a) deriving (Eq, Show)

scoreLine :: String -> JoinList Score String
scoreLine = undefined
