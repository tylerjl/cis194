{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.Char (isDigit)

parseMessage :: String -> LogMessage
parseMessage (x:' ':xs)
    | x == 'I' = LogMessage Info first body
    | x == 'W' = LogMessage Warning first body
    | x == 'E' = LogMessage (Error first) second (trim body)
    where body = tail . dropWhile isDigit $ xs
          first = read . takeWhile isDigit $ xs
          second = read . takeWhile isDigit . dropWhileHeader $ xs
            where dropWhileHeader = tail . snd . span isDigit
          trim = tail . dropWhile isDigit
parseMessage x = Unknown x

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert _ tree@(Node _ (Unknown _) _) = tree
insert m (Leaf) = Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node l c@(LogMessage _ other _) r)
    | other > ts = Node (insert m l) c r
    | otherwise = Node l c (insert m r)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (xs) = foldl buildTree Leaf xs
    where buildTree acc x = insert x acc

inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf) = []
inOrder (Node l m r) = (inOrder l) ++ [m] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (ms) = map printMsg $ filter bigs $ filter errors logs
    where logs = inOrder $ build ms
          bigs (LogMessage (Error n) _ _)
            | n >= 50 = True
            | otherwise = False
          bigs _ = False
          printMsg (LogMessage _ _ s) = s
          printMsg (Unknown s) = s
          errors (LogMessage (Error _) _ _) = True
          errors _ = False
