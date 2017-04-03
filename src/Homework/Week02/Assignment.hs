module Homework.Week02.Assignment
  ( build
  , inOrder
  , insert
  , parse
  , parseMessage
  , whatWentWrong
  , LogMessage(..)
  , MessageTree(..)
  , MessageType(..)
  , TimeStamp
  ) where

import Homework.Week02.Log

-- #1a
parseMessage :: String -> LogMessage
parseMessage ('E':' ':message) =
  let error_code_string:messageText = words message
      error_code = read error_code_string :: Int
  in parseMessage' (Error error_code) (unwords messageText)
parseMessage ('I':' ':message) = parseMessage' Info message
parseMessage ('W':' ':message) = parseMessage' Warning message
parseMessage message = Unknown message

parseMessage' :: MessageType -> String -> LogMessage
parseMessage' messageType message =
  let time_string:message_words = words message
      time = read time_string :: Int
      messageText = unwords message_words
  in LogMessage messageType time messageText

-- #1b
parse :: String -> [LogMessage]
parse logs =
  let messages = lines logs
  in map parseMessage messages

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage messageTree@(Node leftTree nodeMessage rightTree)
  | nodeMessage == logMessage = messageTree
  | time < nodeTime = Node (insert logMessage leftTree) nodeMessage rightTree
  | otherwise = Node leftTree nodeMessage (insert logMessage rightTree)
  where
    time = logTime logMessage
    nodeTime = logTime nodeMessage

logTime :: LogMessage -> Int
logTime (LogMessage _ time _) = time
logTime (Unknown _) = -1

-- insert messageTree
--   | 
-- #3
build :: [LogMessage] -> MessageTree
build logMessages = build' logMessages Leaf

build' :: [LogMessage] -> MessageTree -> MessageTree
build' [] messageTree = messageTree
build' (logMessage:logMessages) messageTree = build' logMessages (insert logMessage messageTree)

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMessage right) = inOrder left ++ [logMessage] ++ inOrder right

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong logMessages =
  let messageTree = build logMessages
      orderedMessages = inOrder messageTree
      errors = filter isProblem orderedMessages
  in map printLogMessage errors

isProblem :: LogMessage -> Bool
isProblem (LogMessage (Error error_code) _ _) = error_code > 50
isProblem _ = False

printLogMessage :: LogMessage -> String
printLogMessage (LogMessage _ _ message) = message
