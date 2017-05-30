{-# LANGUAGE FlexibleInstances #-}

module Homework.Week07.JoinList where

import Homework.Week07.Buffer
import Homework.Week07.Sized
import Homework.Week07.Scrabble
import Homework.Week07.Sized

import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


tag :: Monoid m => JoinList m a -> m
--tag Empty = Empty
tag (Single m _) = m
tag (Append m _ _) = m
tag Empty = mempty

(!!?) :: [a] -> Int -> Maybe a
(!!?) = undefined

jlToList :: Monoid m => JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) l r = Append ((tag l) <> (tag r)) l r

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i Empty = Nothing
indexJ i (Single x j)
  | i == 0 = Just j
  | otherwise = Nothing
indexJ i (Append x l r)
  | i < xx = indexJ i l
  | i >= xx = indexJ (i - xx) r
  where xx = getSize (size (tag l))

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i Empty = Empty
dropJ i jl@(Single x j)
  | i == 0 = jl
  | otherwise = Empty
  where xx = getSize (size x)
dropJ i jl@(Append x l r)
  | i == 0 = jl
  | i < xx = (dropJ i l) +++ r
  | i >= xx = dropJ (i - xx) r
  where xx = getSize (size (tag l))


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i Empty = Empty
takeJ i jl@(Single x j)
  | i <= 0 = Empty
  | otherwise = jl
takeJ i jl@(Append x l r)
  | i < xx = takeJ i l
  | otherwise = l +++ (takeJ (i - xx) r)
  where xx = getSize (size (tag l))

scoreLine :: String -> JoinList Score String
scoreLine s = (Single (scoreString s) s)

instance Buffer (JoinList (Score, Size) String) where
  fromString = undefined
  line = undefined
  numLines = undefined
  replaceLine = undefined
  toString = undefined
  value = undefined


--dropJ :: (Sized b, Monoid b) =>
--  Int -> JoinList b a -> JoinList b a

  
--takeJ :: (Sized b, Monoid b) =>
--  Int -> JoinList b a -> JoinList b a

