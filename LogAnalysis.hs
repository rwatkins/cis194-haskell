{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage "" = Unknown ""
parseMessage s = let (errorType:rest) = words s
                 in case errorType of
                    "E" -> let (severity:timestamp:messageWords) = rest
                           in LogMessage (Error $ read severity)
                                         (read timestamp)
                                         (unwords messageWords)
                    "I" -> let (timestamp:messageWords) = rest
                           in LogMessage Info
                                         (read timestamp)
                                         (unwords messageWords)
                    "W" -> let (timestamp:messageWords) = rest
                           in LogMessage Warning
                                         (read timestamp)
                                         (unwords messageWords)
                    _   -> Unknown s


parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert _ tree@(Node _ (Unknown _) _) = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ timestamp _)
       (Node lTree (LogMessage _ ts _) rTree)
    = insert logMessage $ if timestamp < ts then lTree else rTree

build :: [LogMessage] -> MessageTree
build logs = foldr insert Leaf logs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMessage right) = inOrder left ++
                                       [logMessage] ++
                                       inOrder right

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error severity) _ _) = severity >= 50
isSevere _                                 = False

message :: LogMessage -> String
message (LogMessage _ _ m) = m
message (Unknown m) = m

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs
    = map message $ inOrder $ build $ filter isSevere logs
