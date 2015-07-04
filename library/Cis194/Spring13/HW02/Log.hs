-- CIS 194 Homework 2

-- |Supportive datatypes and functions for LogAnalysis
module Cis194.Spring13.HW02.Log where

-- |Log message type
data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

-- |Simple alias for a timestamp
type TimeStamp = Int

-- |Encapsulates an entire log message
data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

-- |Data structure to store sorted LogMessages
data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file
