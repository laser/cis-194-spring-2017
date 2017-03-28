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
parseMessage s = let w = words s
                 in case w of
                   ("E" : priority : timestamp : message) -> LogMessage (Error (read priority)) (read timestamp) (unwords message)
                   ("I" : timestamp : message) -> LogMessage Info (read timestamp) (unwords message)
                   ("W" : timestamp : message) -> LogMessage Warning (read timestamp) (unwords message)
                   _ -> Unknown (unwords w)

-- #1b
parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) _ = Leaf
insert message Leaf = Node Leaf message Leaf
insert m@(LogMessage _ timestamp _) (Node left o@(LogMessage _ timestamp2 _) right)
        | timestamp < timestamp2 = Node (insert m left) o right
        | timestamp > timestamp2 = Node left o (insert m right)

-- #3
build :: [LogMessage] -> MessageTree
build ms = build' ms Leaf
        where build' [] acc = acc
              build' (m:ms) acc = build' ms (insert m acc)

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = (inOrder left) ++ [m] ++ (inOrder right)

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map message) . inOrder . build . (filter highPriority)
        where highPriority m = (priority m) > 50
              priority (LogMessage (Error p) _ _) = p
              priority _ = 0
              message (LogMessage _ _ m) = m
              message (Unknown m) = m
