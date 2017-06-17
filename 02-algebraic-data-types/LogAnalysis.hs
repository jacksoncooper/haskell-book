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
  where logMessageList = words logMessage

parse :: String -> [LogMessage]
parse logMessages = map parseMessage (lines logMessages)
