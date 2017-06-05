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
import Data.Char

import Homework.Week09.AParser

-- #1
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []
        
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- #2
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

-- #3
type Ident = String

data Atom = N Integer
          | I Ident
  deriving (Eq, Show)

data SExpr = A Atom
           | Comb [SExpr]
  deriving (Eq, Show)

parseSExpr :: Parser SExpr
parseSExpr = 
  trim (char '(' *> (Comb <$> oneOrMore parseSExpr) <* char ')')
  <|> trim (A <$> parseAtom)

parseAtom :: Parser Atom
parseAtom = trim ((I <$> ident) <|> (N <$> posInt))

trim :: Parser a -> Parser a
trim p = spaces *> p <* spaces