{-# LANGUAGE FlexibleInstances #-}

module Homework.Week07.JoinList (
  tag,
  indexJ,
  (+++),
  (!!?),
  jlToList,
  dropJ,
  takeJ,
  scoreLine,
  Sized(..),
  JoinList(..)
) where

import Homework.Week07.Buffer
import Homework.Week07.Scrabble
import Homework.Week07.Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a) deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(!!?) :: [a] -> Int -> Maybe a
(!!?) [] _ = Nothing
(!!?) _ i | i < 0 = Nothing
(!!?) (x:xs) 0 = Just x
(!!?) (x:xs) i = xs !!? (i-1)

jlToList :: Monoid m => JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) list1 list2 = Append (mappend (tag list1) (tag list2)) list1 list2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append _ l1 l2)
  | i < index1 = indexJ i l1 
  | otherwise = indexJ i l2
  where index1 = getSize . size . tag $ l1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl | i < 0 = jl

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ = undefined

scoreLine :: String -> JoinList Score String
scoreLine = undefined

instance Buffer (JoinList (Score, Size) String) where
  fromString = undefined
  line = undefined
  numLines = undefined
  replaceLine = undefined
  toString = undefined
  value = undefined
