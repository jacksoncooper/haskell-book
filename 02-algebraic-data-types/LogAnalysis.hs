-- https://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf

{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage logMessage = case logMessageList of
    ("I":timeStamp:body)          -> LogMessage Info (read timeStamp) (unwords body)
    ("W":timeStamp:body)          -> LogMessage Warning (read timeStamp) (unwords body)
    ("E":severity:timeStamp:body) -> LogMessage (Error (read severity)) (read timeStamp) (unwords body)
    _                             -> Unknown logMessage
  where
    logMessageList = words logMessage

parse :: String -> [LogMessage]
parse logMessages = map parseMessage (lines logMessages)

-- Exercise 2

-- TODO: Yikes.

insert :: LogMessage -> MessageTree -> MessageTree
insert logMessage messageTree = case (timeStamp < nodeTimeStamp, logMessage, messageTree) of
    (_, Unknown _, _)                                      -> messageTree
    (_, _, Leaf)                                           -> (Node Leaf logMessage Leaf)
    (True, _, (Node leftChild nodeLogMessage rightChild))  -> (Node (insert logMessage leftChild) nodeLogMessage rightChild)
    (False, _, (Node leftChild nodeLogMessage rightChild)) -> (Node leftChild nodeLogMessage (insert logMessage rightChild))
  where
    timeStamp     = getTimeStamp logMessage
    nodeTimeStamp = getTimeStamp (getTreeData messageTree)

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ timeStamp _ ) = timeStamp

getTreeData :: MessageTree -> LogMessage
getTreeData (Node _ logMessage _) = logMessage
