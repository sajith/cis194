{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import           Log

parseMessage :: String -> LogMessage
parseMessage message =
    case msgType of
        "I" -> LogMessage Info infoTs infoMsg
        "W" -> LogMessage Warning warnTs warnMsg
        "E" -> LogMessage (Error errNum) errTs errMsg
        _   -> Unknown message
    where
        msgWrds = words message
        msgType = head msgWrds
        infoTs  = read $ msgWrds !! 1
        infoMsg = unwords $ drop 2 msgWrds
        warnTs  = read $ msgWrds !! 1
        warnMsg = unwords $ drop 2 msgWrds
        errNum  = read $ msgWrds !! 1
        errTs   = read $ msgWrds !! 2
        errMsg  = unwords $ drop 3 msgWrds

parse :: String -> [LogMessage]
parse file = map parseMessage $ lines file

------------------------------------------------------------

timeStamp :: LogMessage -> TimeStamp
timeStamp (LogMessage _ ts _) = ts
timeStamp (Unknown _ ) = error "unknown message type"

-- TODO: wrong?
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree             = tree
insert message Leaf                 = Node Leaf message Leaf
insert message (Node left m' right) = if timeStamp m' > timeStamp message
                                      then Node left m' (insert message right)
                                      else Node (insert message left) m' right

build :: [LogMessage] -> MessageTree
build messages = build' Leaf messages where
    build' tree []     = tree
    build' tree (m:ms) = build' (insert m tree) ms

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map getMessage $ filter (\m -> isError m && hasSeverity 50 m) messages
    where
        getMessage :: LogMessage -> String
        getMessage (LogMessage _ _ str) = str
        getMessage (Unknown str)        = str
        
        isError :: LogMessage -> Bool
        isError (LogMessage (Error _) _ _) = True
        isError _                          = False

        hasSeverity :: Int -> LogMessage -> Bool
        hasSeverity lvl (LogMessage (Error n) _ _) = if n >= lvl then True else False
        hasSeverity _ _                            = False
