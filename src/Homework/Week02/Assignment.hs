module Homework.Week02.Assignment (
  build,
  inOrder,
  insert,
  parse,
  parseMessage,
  whatWentWrong,
  LogMessage(..),
  MessageTree(..),
  MessageType(..),
  TimeStamp
) where

import Homework.Week02.Log

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

import qualified Text.Parsec as Parsec (parse)
import Text.Parsec (many1, digit, anyChar, space, char, (<|>), ParseError)
import Text.Parsec.String (Parser)

-- #1a
infoMessage :: Parser LogMessage
infoMessage = do
  (ts, msg) <- char 'I' >> space *> timeStampedMessage
  return $ LogMessage Info ts msg

warningMessage :: Parser LogMessage
warningMessage = do
  (ts, msg) <- char 'W' >> space *> timeStampedMessage
  return $ LogMessage Warning ts msg

errorMessage :: Parser LogMessage
errorMessage = do
  c <- char 'E' >> space *> int <* space
  (ts, msg) <- timeStampedMessage
  return $ LogMessage (Error c) ts msg

unknownMessage :: Parser LogMessage
unknownMessage = allChars >>= return . Unknown

logMessage :: Parser LogMessage
logMessage = infoMessage <|> warningMessage <|> errorMessage <|> unknownMessage

parseMessage :: String -> LogMessage
parseMessage msg = case Parsec.parse logMessage "" msg of
                     (Right m) -> m
                     (Left _) -> error "Something has gone terribly wrong"

-- #1b
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ mts _) (Node left mt@(LogMessage _ mtts _) right)
  | mts < mtts = Node (insert m left) mt right
  | otherwise = Node left mt (insert m right)

-- #3
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder (Node left mt right) = inOrder left ++ [mt] ++ inOrder right
inOrder _ = []

-- #5
isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error c) _ _) = c > 50
isRelevant _ = False

toMessage :: LogMessage -> String
toMessage (LogMessage _ _ msg) = msg
toMessage (Unknown msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = fmap toMessage . filter isRelevant . inOrder . build

-- parser combinator helpers
int :: Parser Int
int = many1 digit >>= return . read

allChars :: Parser String
allChars = many1 anyChar >>= return

timeStampedMessage :: Parser (TimeStamp, String)
timeStampedMessage = do
  ts <- int <* space
  msg <- allChars
  return (ts, dropWhileEnd isSpace msg)

