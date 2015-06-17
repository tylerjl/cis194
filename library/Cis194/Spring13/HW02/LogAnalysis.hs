{-# OPTIONS_GHC -Wall #-}

module Cis194.Spring13.HW02.LogAnalysis
    ( parseMessage
) where

import Cis194.Spring13.HW02.Log

parseMessage :: String -> LogMessage
parseMessage m@([]) = Unknown m
parseMessage m@(c:cs)
    | c == 'I' = LogMessage Info 0 cs
    | otherwise = Unknown m

-- parseTS :: String -> Maybe Int
-- parseTS m = case matchRegex pattern m of
--                  Just i -> 
--     where pattern = mkRegex " +(\d+) +"
