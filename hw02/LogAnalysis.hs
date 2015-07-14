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
timeStamp (Unknown _ )        = -1

-- TODO: wrong?
insert :: LogMessage -> MessageTree -> MessageTree
insert message tree =
    if (timeStamp message == -1)
    then tree
    else case tree of
            Leaf -> Node Leaf m' Leaf
            Node left l right ->
                if timeStamp l > timeStamp message
                then Node left m' (insert message right)
                else Node (insert message left) m' right

