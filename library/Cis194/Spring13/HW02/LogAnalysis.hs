{-# OPTIONS_GHC -Wall #-}

module Cis194.Spring13.HW02.LogAnalysis
    ( parse
    , parseMessage
    , insert
    , build
    , inOrder
    , whatWentWrong
) where

import Data.Monoid ((<>))
import Data.List (foldl')
import Control.Applicative ((<|>))
import qualified Text.Parsec as P
import Text.Parsec.Char (anyChar, char, space, digit)
import Text.Parsec.String (Parser)
import Cis194.Spring13.HW02.Log

-- |Parse [LogMessage] from a raw string
parse :: String -> [LogMessage]
parse = fmap (parseMessage) . lines

-- |Parse a LogMessage from a raw string.
parseMessage :: String -> LogMessage
parseMessage s =
    case regularParse parseLog s of
         Right p -> p
         Left  _ -> Unknown s

-- |Generic parsing wrapper
regularParse :: Parser a -> String -> Either P.ParseError a
regularParse p = P.parse p ""

-- |High-level parser for the log as a whole
parseLog :: Parser LogMessage
parseLog = LogMessage <$> (logType <* space)
                      <*> (timeStamp <* space)
                      <*> (P.manyTill anyChar P.eof)

-- |Parse the log type
logType :: Parser MessageType
logType = Info             <$  (char 'I')
      <|> Warning          <$  (char 'W')
      <|> Error <$> (read) <$> (char 'E' *> P.many1 space *> P.many1 digit)

-- |Parse message timestamp
timeStamp :: Parser TimeStamp
timeStamp = read <$> P.many1 digit

{-
 - Insert LogMessage into MessageTree, return resultant MessageTree.
 - Tree should be sorted as a binary search tree.
 - Unknown LogMessage arguments should be ignored, returning the original
 - MessageTree.
 -}
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t                = t
insert m (Leaf)                     = Node Leaf m Leaf
insert n (Node lt m@(Unknown _) rt) = Node lt m (insert n rt)
insert n@(LogMessage _ ts _) (Node lt m@(LogMessage _ v _) rt)
    | v < ts    = Node lt m (insert n rt)
    | otherwise = Node (insert n lt) m rt

-- |Construct a MessageTree from [LogMessage]
build :: [LogMessage] -> MessageTree
build = foldl' (flip insert) Leaf

-- |Traverse a MessageTree in-order to return a sorted [LogMessage]
inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf)       = []
inOrder (Node l v r) = inOrder l <> [v] <> inOrder r

-- |Returns list of [String] with all error log messages with severity 50
-- |or greater sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = fmap (msgBody) . inOrder . build . filter (isCritical)

-- |Print the message body of a LogMessage
msgBody :: LogMessage -> String
msgBody (Unknown b)        = b
msgBody (LogMessage _ _ b) = b

-- |Determine whether a LogMessage is an Error
isCritical :: LogMessage -> Bool
isCritical (LogMessage (Error s) _ _)
    | s >= 50   = True
    | otherwise = False
isCritical _ = False
