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
data Stream a = Stream a -- replace this with your own definition; this one is wrong

streamToList :: Stream a -> [a]
streamToList = undefined

-- instance Show a => Show (Stream a) where
--   show = ???

-- #4
streamRepeat :: a -> Stream a
streamRepeat = undefined

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = undefined

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed = undefined

-- #5
nats :: Stream Integer
nats = undefined

ruler :: Stream Integer
ruler = undefined
