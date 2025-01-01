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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert logMsg@(LogMessage _ ts _) msgTree =
  case msgTree of
    Leaf -> Node Leaf logMsg Leaf
    (Node lTree nodeLogMsg rTree) -> case nodeLogMsg of
      (Unknown _) -> msgTree
      (LogMessage _ nodeTs _) ->
        if ts < nodeTs
          then
            Node (insert logMsg lTree) nodeLogMsg rTree
          else
            Node lTree nodeLogMsg (insert logMsg rTree)

-- Exercise 3

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x : xs) = insert x (build xs)

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lTree logMsg rTree) = (inOrder lTree) ++ [logMsg] ++ (inOrder rTree)

-- Exercise 5

sevAtLeast :: Int -> LogMessage -> Bool
sevAtLeast n (LogMessage (Error sev) _ _) = sev >= n
sevAtLeast _ _ = False

getMsg :: LogMessage -> String
getMsg (LogMessage _ _ msg) = msg
getMsg _ = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (getMsg) . filter (sevAtLeast 50) . inOrder . build
