module Homework.Week06.Assignment (
  fib,
  fibs1,
  fibs2,
  streamToList,
  streamRepeat,
  streamMap,
  streamFromSeed,
  nats,
  ruler,
  Stream(..)
) where

-- #1a
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- #2
fibs2 :: [Integer]
fibs2 = 0 : 1 : fibs2' 0 1

fibs2' :: Integer -> Integer -> [Integer]
fibs2' x1 x2 = 
  let next = x1 + x2
   in next : fibs2' x2 next

-- #3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- #4
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Stream s (streamFromSeed f (f s))

-- #5
nats :: Stream Integer
nats = undefined

ruler :: Stream Integer
ruler = undefined
