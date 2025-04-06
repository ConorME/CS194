{-# OPTIONS_GHC -Wall #-}
module ZeroTwo where

import Log

parseMessage :: String -> LogMessage
parseMessage line = case words line of
    ("I":timestamp:msg) -> LogMessage Info (read timestamp) (unwords msg)
    ("W":timestamp:msg) -> LogMessage Warning (read timestamp) (unwords msg)
    ("E":level:timestamp:msg) -> LogMessage (Error (read level)) (read timestamp) (unwords msg)
    _ -> Unknown line

parse :: String -> [LogMessage]
parse content = map parseMessage (lines content)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message@(LogMessage _ time _) (Node left nodeMessage@(LogMessage _ nodeTime _) right)
   | time <  nodeTime = Node (insert message left) nodeMessage right
   | otherwise = Node left nodeMessage (insert message right)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build messages = foldl (flip insert) Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = (inOrder left) ++ [message] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map getMessage severeErrors where
   severeErrors = filter isSevereError sortedMessages
   sortedMessages = inOrder (build messages)

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error severity) _ _) = severity >= 50
isSevereError _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ message) = message
getMessage (Unknown message) = message
