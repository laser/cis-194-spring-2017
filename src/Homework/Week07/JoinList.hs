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
  JoinList(..),
) where

import Homework.Week07.Buffer
import Homework.Week07.Scrabble
import Homework.Week07.Sized
import Homework.Week07.Editor

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
(!!?) (x:_) 0 = Just x
(!!?) (_:xs) i = xs !!? (i-1)

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
  | otherwise = indexJ (i - index1) l2
  where index1 = getSize . size . tag $ l1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ n (Append m l r)
  | n >= sizeM = Empty
  | n == sizeL = r
  | n > sizeL = dropJ n r
  | n < sizeL = 
    let newL = dropJ n l
     in Append (mappend (tag newL) (tag r)) newL r
  where sizeM = getSize . size $ m
        sizeL = getSize . size . tag $ l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ n jl@(Append m l r)
  | n >= sizeM = jl
  | n == sizeL = l
  | n < sizeL = takeJ n l
  | n > sizeL = 
    let newR = takeJ (n - sizeL) r
     in Append (mappend (tag l) (tag newR)) l newR
  where sizeM = getSize . size $ m
        sizeL = getSize . size . tag $ l

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldr (\ x -> (+++) (Single (scoreString x, 1) x)) Empty . lines
  line = indexJ
  numLines = getSize . snd . tag
  replaceLine n string jl = takeJ (n - 1) jl +++ fromString string +++ dropJ n jl
  value = getScore . fst . tag

main :: IO ()
main =
  let defaultString = unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
      defaultBuffer = fromString defaultString :: (JoinList (Score, Size) String)
  in runEditor editor defaultBuffer