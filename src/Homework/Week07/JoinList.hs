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

import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a) deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag  Empty         = mempty
tag (Single x _)   = x
tag (Append x _ _) = x

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: Monoid m => JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

intFromJ :: (Sized b, Monoid b) => JoinList b a -> Int
intFromJ = (getSize . size . tag)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                = Nothing
indexJ i _            | i < 0 = Nothing
indexJ i (Single m a) | i > 0 = Nothing
indexJ 0 (Single m a)         = Just a
indexJ i (Append _ jla jlb)
  | i < intFromJ jla = indexJ i jla
  | otherwise        = indexJ (i - intFromJ jla) jlb

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl
  | i <= 0                 = jl
  | i >= intFromJ jl       = Empty
dropJ i (Append j jla jlb) = (dropJ i jla) +++ dropJ (i - intFromJ jla) jlb

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i jl
  | i <= 0                 = Empty
  | i >= intFromJ jl       = jl
takeJ i (Append _ jla jlb) = takeJ i jla +++ takeJ (i - intFromJ jla) jlb

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

fromStringLines :: [String] -> JoinList (Score, Size) String
fromStringLines [] = Empty
fromStringLines [a] = Single (scoreString a, Size 1) a
fromStringLines (a:as) = fromStringLines [a] +++ fromStringLines as

extractScore :: Score -> Int
extractScore (Score i) = i

instance Buffer (JoinList (Score, Size) String) where
  fromString str = fromStringLines (lines str)
  line = indexJ
  numLines = getSize . snd . tag
  replaceLine i str jl = takeJ i jl +++ fromString str +++ dropJ (i + 1) jl
  toString Empty = ""
  toString (Single _ str) = str
  toString (Append _ jla jlb) = toString jla ++ "\n" ++ toString jlb
  value jl = (extractScore . fst . tag) jl
