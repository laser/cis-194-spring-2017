module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Homework.Week08.AParser
import Control.Applicative
import Data.Char

-- #1
first :: (a -> b) -> (a,c) -> (b,c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f (Parser runParser) = Parser $ ((first f) <$>) . runParser

-- #2
instance Applicative Parser where
  pure a = Parser f
    where f xs = Just(a, xs)
  Parser rp1 <*> Parser rp2 = Parser f
        where
          f xs = case (rp1 xs) of
            Just (rf1, rxs) -> case rp2 rxs of
              Just (rv2, s) -> Just (rf1 rv2, s)
              otherwise -> Nothing
            otherwise -> Nothing

-- #3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = const () <$> abParser

intPair :: Parser [Integer]
intPair = (\(a, b) -> [a,b]) <$> ((,) <$> posInt <* char ' ' <*> posInt)

-- #4
instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser rp1 <|> Parser rp2 = Parser $ (<|>) <$> rp1 <*> rp2

-- #5
intOrUppercase :: Parser ()
intOrUppercase = (const () <$> posInt) <|> (const () <$> satisfy isUpper)
