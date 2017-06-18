module Homework.Week09.Assignment (
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

