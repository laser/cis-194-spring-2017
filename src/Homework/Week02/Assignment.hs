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

-- #1a

int :: String -> Maybe (Int, String)
int line = case words line of
             (s:ss) -> Just (read s :: Int, unwords ss)
             _ -> Nothing

parseMessage' :: MessageType -> String -> LogMessage
parseMessage' t line = case int line of
                         Just (ts, l) -> LogMessage t ts l
                         Nothing -> Unknown line

parseMessage :: String -> LogMessage
parseMessage ('I':line) = parseMessage' Info line
parseMessage ('W':line) = parseMessage' Warning line
parseMessage ('E':line) = case int line of
                            Just (c, line') -> parseMessage' (Error c) line'
                            Nothing -> Unknown line
parseMessage line = Unknown line

-- #1b
parse :: String -> [LogMessage]
parse = undefined

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert = undefined

-- #3
build :: [LogMessage] -> MessageTree
build = undefined

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
