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
import Prelude
import Data.List (intercalate)

-- #1a
parseMessage :: String -> LogMessage
parseMessage ('E':s) = do
  let fragment = words s
  let timestampAndSeverity = take 2 fragment
  LogMessage (Error (read (head timestampAndSeverity) :: Int)) (read (last timestampAndSeverity) :: Int) (intercalate " " (drop 2 fragment))

parseMessage ('W':s) = do
  let fragment = words s
  LogMessage Warning (read (head fragment) :: Int) (unwords (tail fragment))

parseMessage ('I':s) = do
  let fragment = words s
  LogMessage Info (read (head fragment) :: Int) (unwords (tail fragment))

parseMessage message = Unknown message

-- #1b
parse :: String -> [LogMessage]
parse messages = map parseMessage (lines messages)

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert m1@(LogMessage _ t1 _) tree@(Node left m2@(LogMessage _ t2 _) right)
  | t1 > t2 = Node left m2 (Node Leaf m1 Leaf)
  | t1 < t2 = Node (Node Leaf m1 Leaf) m2 right
  | otherwise = tree

-- #3
build :: [LogMessage] -> MessageTree
build [Unknown _] = Leaf
build messages = foldl (flip insert) Leaf messages

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder (Node left message Leaf) = (inOrder left) ++ [message]
inOrder (Node Leaf message right) = [message] ++ (inOrder right)
inOrder (Node left message right) = (inOrder left) ++ [message] ++ (inOrder right)
inOrder Leaf = []

-- #5
severe :: LogMessage -> Bool
severe (LogMessage (Error severity) _ _) | severity >= 50 = True
severe _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map (\(LogMessage _ _ m) -> m) (inOrder (build (filter severe messages)))
