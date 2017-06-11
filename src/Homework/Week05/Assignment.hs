module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  Expr(..),
  MinMax(..),
  Mod7(..)
) where

import Homework.Week05.ExprT
import Homework.Week05.Parser

-- #1
eval :: ExprT -> Integer
eval x = case x of
  (Lit l) -> l
  (Mul x y) -> (eval x) * (eval y)
  (Add x y) -> (eval x) + (eval y)

-- #2
evalStr :: String -> Maybe Integer
evalStr x = case me of
  Nothing -> Nothing
  (Just exp) -> Just $ eval exp
  where me = parseExp Lit Add Mul x

-- #3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- #4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x = if x <= 0 then False else True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . ((flip mod) 7)
  add (Mod7 x) (Mod7 y) = Mod7 $ ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 $ ((x * y) `mod` 7)
