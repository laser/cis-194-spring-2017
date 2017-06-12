module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase,
  satisfy,
  posInt,
  Parser
) where

import Homework.Week08.AParser
import Control.Applicative

import Data.Char

-- #1
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a,c)

instance Functor Parser where
  fmap f p = Parser (fmap (fmap (first f)) (runParser p))
  
-- #2
instance Applicative Parser where
  pure a = Parser (\s -> Just (a,s))
  p <*> g = Parser (\s ->
    case runParser p s of
      Nothing       -> Nothing
      Just (f',s')  -> runParser (fmap f' g) s')

-- #3
abParser :: Parser (Char, Char)
abParser = Parser (\s ->
  case s of
    ('a':'b':s') -> Just (('a', 'b'), s')
    _ -> Nothing)

abParser' :: Parser (Char, Char)
abParser' = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (const (const  ())) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\a _ b -> [a,b]) <$> posInt <*> char ' ' <*> posInt

-- #4
instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p <|> Parser g = Parser (pure (<|>) <*> p <*> g)

-- #5
intOrUppercase :: Parser ()
intOrUppercase = (unit' <$> posInt) <|> (unit' <$> (satisfy isUpper))
  where unit' = const ()

