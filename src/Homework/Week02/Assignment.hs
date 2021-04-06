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
parseMessage :: String -> LogMessage
parseMessage str = parseTokens (words str)

parseTokens ("I":ts:rest)     = LogMessage  Info                     (read ts :: TimeStamp) (unwords rest)
parseTokens ("W":ts:rest)     = LogMessage  Warning                  (read ts :: TimeStamp) (unwords rest)
parseTokens ("E":eNo:ts:rest) = LogMessage (Error (read eNo :: Int)) (read ts :: TimeStamp) (unwords rest)
parseTokens tokens            = Unknown (unwords tokens)

-- #1b yes
parse :: String -> [LogMessage]
parse string = map parseMessage (lines string)

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ lmts _) (Node left tlm@(LogMessage _ mtts _) right) =
  if lmts < mtts
    then (Node (insert lm left) tlm right)
    else (Node left tlm (insert lm right))

-- #3
build :: [LogMessage] -> MessageTree
build lms = foldr insert Leaf lms

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                 = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right

-- #5 --
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = map extractMessage ((inOrder . build . (filter isSevErr)) lms)

isSevErr :: LogMessage -> Bool
isSevErr (LogMessage (Error sev) _ _) = sev >= 50
isSevErr _                            = False

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ m) = m
