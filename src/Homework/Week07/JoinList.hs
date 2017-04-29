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
import Data.List

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a) deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

(!!?) :: [a] -> Int -> Maybe a
(!!?) = undefined

jlToList :: Monoid m => JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ s) = [s]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty a = a
(+++) a Empty = a
(+++) a b = Append (tag a <> tag b) a b

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Single _ a)
  | i <= 0 = Just a
indexJ i (Append m l r)
  | i < 0 || i > size0 = Nothing
  | i < sizeL = indexJ i l
  | otherwise = indexJ (i - sizeL) r
  where size0 = getSize . size $ m
        sizeL = getSize . size . tag $ l
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n l@(Single _ _)
  | n <= 0 = l
dropJ n l@(Append m l1 l2)
  | n >= size0 = Empty
  | n >= size1 = dropJ (n - size1) l2
  | n > 0 = dropJ n l1 +++ l2
  | otherwise = l
  where size0 = getSize . size $ m
        size1 = getSize . size . tag $ l1
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n l@(Single _ _)
  | n > 0 = l
takeJ n l@(Append m l1 l2)
  | n >= size0 = l
  | n >= size1 = l1 +++ takeJ (n - size1) l2
  | n > 0 = takeJ n l1
  where size0 = getSize . size $ m
        size1 = getSize . size . tag $ l1
takeJ _ _ = Empty

scoreLine :: String -> JoinList Score String
scoreLine s = (Single (scoreString s) s)

scoreAndLengthLine :: String -> JoinList (Score, Size) String
scoreAndLengthLine s = Single (scoreString s, Size (length s)) s

instance Buffer (JoinList (Score, Size) String) where
  fromString = (foldl (+++) Empty) . (map scoreAndLengthLine) . lines
  line = indexJ
  numLines = getSize . size . tag
  replaceLine n s jl = (takeJ (n) jl) +++ scoreAndLengthLine s +++ (dropJ (n+1) jl)
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ s1 s2) = intercalate "\n" $ map toString [s1, s2]
  value = getScore . fst . tag
