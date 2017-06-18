module Homework.Week09.Assignment (
  upper,
  zeroOrMore,
  oneOrMore,
  spaces,
  ident,
  parseSExpr,
  Ident(..),
  Atom(..),
  SExpr(..)
) where

import Control.Applicative
import Data.Char (isUpper, isSpace, isAlpha, isAlphaNum)

import Homework.Week09.AParser

-- #1
-- isUpper :: Char -> Bool
upper :: Parser Char
upper = satisfy isUpper

-- Hint: To parse one or more occurences of p, run p
--       once and then parse zero or more occurences
--       of p. To parse zero or more occurences of p,
--       try parsing one or more; if that fails,
--       return the empty list.

--
-- zeroOrMore upper :: Parser [Char]
-- runParser (zeroOrMore upper) "ABCd" :: Maybe ([Char], String)
-- runParser :: String -> Maybe (a, String)
-- 
-- <$> :: (a -> b) -> f a -> f b
-- pure :: a -> f a
-- <*> :: f (a -> b) -> f a -> f b
-- <|> :: f a -> f a -> f a
-- 
-- (:)                                  :: a -> [a] -> [a]
-- ((<$>) (:))                          :: f a -> f ([a] -> [a])
-- ((:) <$> upper)                      :: Parser ([Char] -> [Char])
-- ((<*>) ((:) <$> upper))              :: Parser [Char] -> Parser [Char]
-- ((:) <$> upper <*> zeroOrMore upper) :: Parset [Char] -- Aha!

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> (pure [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

space :: Parser Char
space = satisfy isSpace

-- #2
spaces :: Parser String
spaces = zeroOrMore space

-- ident is basically the same as oneOrMore

alpha :: Parser Char
alpha = satisfy isAlpha

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

ident :: Parser String
ident = (:) <$> alpha <*> zeroOrMore alphaNum

-- #3
type Ident = String

data Atom = N Integer
          | I Ident
  deriving (Eq, Show)

data SExpr = A Atom
           | Comb [SExpr]
  deriving (Eq, Show)

integer :: Parser Atom
integer = N <$> posInt

identifier :: Parser Atom
identifier = I <$> ident

atom :: Parser SExpr
atom = A <$> (integer <|> identifier)

openParen :: Parser Char
openParen = char '('

closeParen :: Parser Char
closeParen = char ')'

comb :: Parser SExpr
comb = Comb <$> (openParen *> (oneOrMore parseSExpr) <* closeParen)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (atom <|> comb) <* spaces

