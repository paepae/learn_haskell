{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Excercise 1

parseMessage :: String -> LogMessage
parseMessage str =
  case take 4 wordList of
    ("I" : ts : _ : _) -> LogMessage Info (read ts) (unwords (drop 2 wordList))
    ("W" : ts : _ : _) -> LogMessage Warning (read ts) (unwords (drop 2 wordList))
    ("E" : sev : ts : _ : _) -> LogMessage (Error (read sev)) (read ts) (unwords $ drop 3 wordList)
    _ -> Unknown str
  where
    wordList = words str

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2
-- Not implemented yet

-- Exercise 3
-- Not implemented yet

-- Exercise 4
-- Not implemented yet

-- Exercise 5
-- Not implemented yet

-- Exercise 6
-- Not implemented yet
