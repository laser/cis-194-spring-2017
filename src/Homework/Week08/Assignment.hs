module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Homework.Week08.AParser
import Control.Applicative

-- #1
first :: (a -> b) -> (a,c) -> (b,c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f parser = Parser newFn
    where 
      newFn string = 
        case parsed of
          Nothing -> Nothing
          Just parsedPair -> Just (first f parsedPair)
        where parsed = runParser parser string

-- #2
instance Applicative Parser where
  pure a = Parser (\xs -> Just (a, xs))
  p1 <*> p2 = Parser f
    where 
      f string =
        case parsedOne of
          Nothing -> Nothing
          Just (fp1, xs) -> runParser (fmap fp1 p2) xs
        where parsedOne = runParser p1 string

-- #3
abParser :: Parser (Char, Char)
abParser = undefined

abParser_ :: Parser ()
abParser_ = undefined

intPair :: Parser [Integer]
intPair = undefined

-- #4
instance Alternative Parser where
  empty = undefined
  _ <|> _ = undefined

-- #5
intOrUppercase :: Parser ()
intOrUppercase = undefined
