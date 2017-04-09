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
parseMessage s = parseMessage' (words s)
  where parseMessage' :: [String] -> LogMessage
        parseMessage' ("E" : e : t : message) = LogMessage (Error (read e)) (read t) (unwords message)
        parseMessage' ("I" : t : message) = LogMessage Info (read t) (unwords message)
        parseMessage' ("W" : t : message) = LogMessage Warning (read t) (unwords message)
        parseMessage' w =  Unknown (unwords w)

-- #1b
parse :: String -> [LogMessage]
parse s = parse' (lines s)
  where parse' :: [String] -> [LogMessage]
        parse' [] = []
        parse' (line : rest) = (parseMessage line) : parse' rest

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t 
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node left m'@(LogMessage _ t' _) right)
  | t <= t' = (Node (insert m left) m' right)
  | t > t'  = (Node left m' (insert m right))

-- #3
build :: [LogMessage] -> MessageTree
build messages = build' Leaf messages
  where build' :: MessageTree -> [LogMessage] -> MessageTree
        build' acc [] = acc
        build' acc (message : rest) = build' (insert message acc) rest

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = (inOrder left) ++ (message : (inOrder right))

-- #5
whatWentWrong' :: [LogMessage] -> [String]
whatWentWrong' [] = []
whatWentWrong' ((LogMessage e@(Error n) _ m) : rest)
  | n >= 50   = (m : (whatWentWrong' rest))
  | otherwise = whatWentWrong' rest
whatWentWrong' (_ : rest) = whatWentWrong' rest

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong messages = whatWentWrong' (inOrder (build messages))
