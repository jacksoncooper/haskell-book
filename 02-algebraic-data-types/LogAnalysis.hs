-- https://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf

{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage logMessage = case words logMessage of
    ("I":timeStamp:body)          -> LogMessage Info (read timeStamp) (unwords body)
    ("W":timeStamp:body)          -> LogMessage Warning (read timeStamp) (unwords body)
    ("E":severity:timeStamp:body) -> LogMessage (Error (read severity)) (read timeStamp) (unwords body)
    _                             -> Unknown logMessage

parse :: String -> [LogMessage]
parse logMessages = map parseMessage $ lines logMessages

-- Exercise 2
-- Took a hint from @bschwb. First solution worked but pattern matching against a triple using a case expression was really redundant.
-- GHC -Wall warns that pattern matches are non-exhaustive because "Patterns not matched: (LogMessage _ _ _) (Node _ (Unknown _) _)". This should be a nonissue because a MessageTree should never contain an unknown LogMessage.

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage Leaf = (Node Leaf logMessage Leaf)
insert logMessage@(LogMessage _ timeStamp _) (Node leftChild nodeLogMessage@(LogMessage _ nodeTimeStamp _) rightChild)
    | timeStamp > nodeTimeStamp = (Node leftChild nodeLogMessage (insert logMessage rightChild))
    | otherwise                 = (Node (insert logMessage leftChild) nodeLogMessage rightChild)
