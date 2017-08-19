module LogAnalysis where

import Log

parseMessage :: String -> LogMessage

parseMessage m = let wordList = words m in
                  case wordList of
                   ("I" : t : xs )   -> LogMessage Info (read t) $ unwords xs
                   ("W" : t : xs )   -> LogMessage Warning (read t) $ unwords xs
                   ("E" : c : t: xs) -> LogMessage (Error (read c)) (read t) $ unwords xs
                   otherwise         -> Unknown m

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

getInfoLogMessage xs = LogMessage Info 
