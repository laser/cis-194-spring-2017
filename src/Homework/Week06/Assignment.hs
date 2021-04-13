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
fib n
  | n <= 0    = 0
  | n == 1    = 1
  | otherwise = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- #2 yes
fibs2 :: [Integer]
fibs2 = map fst (scanl (\(fNMinusTwo, fNMinusOne) _ -> (fNMinusOne, fNMinusTwo + fNMinusOne)) (0,1) (repeat 0))


-- #3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = (x:streamToList xs)

-- instance Show a => Show (Stream a) where
--   show = ???

-- #4
streamRepeat :: a -> Stream a
streamRepeat x = (Cons x (streamRepeat x))

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = (Cons (f x) (streamMap f xs))

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = (Cons x (streamFromSeed f (f x)))

-- #5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = undefined
