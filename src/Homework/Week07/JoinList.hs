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
tag = undefined

(!!?) :: [a] -> Int -> Maybe a
(!!?) = undefined

jlToList :: Monoid m => JoinList m a -> [a]
jlToList = undefined

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) = undefined

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ = undefined

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ = undefined

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

tag :: Monoid m => JoinList m a -> m
--tag Empty = Empty
tag (Single m _) = m
tag (Append m _ _) = m
tag Empty = mempty

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) l r = Append ((tag l) <> (tag r)) l r

indexJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> Maybe a  
indexJ i Empty = Nothing
indexJ i (Single  x j)
  | i < 0 = Nothing
  | i == getSize (size x) = Just j
-- indexJ i (Append x l r)
--   | i < x = indexJ i l
  -- | i >= x = indexJ (i - x) r

--dropJ :: (Sized b, Monoid b) =>
--  Int -> JoinList b a -> JoinList b a

  
--takeJ :: (Sized b, Monoid b) =>
--  Int -> JoinList b a -> JoinList b a

