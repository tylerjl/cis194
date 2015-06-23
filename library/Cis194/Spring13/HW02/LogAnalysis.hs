{-# OPTIONS_GHC -Wall #-}

module Cis194.Spring13.HW02.LogAnalysis
    ( parseMessage
) where

import Control.Applicative ((<|>))
import Text.Parsec (eof, parse, many1, manyTill, ParseError)
import Text.Parsec.Char (anyChar, char, space, digit)
import Text.Parsec.String (Parser)
import Cis194.Spring13.HW02.Log

-- |Parse a LogMessage from a raw string.
parseMessage :: String -> LogMessage
parseMessage s =
    case regularParse parseLog s of
         Right p -> p
         Left  _ -> Unknown s

-- |Generic parsing wrapper
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

-- |High-level parser for the log as a whole
parseLog :: Parser LogMessage
parseLog = LogMessage <$> (logType <* space)
                      <*> (timeStamp <* space)
                      <*> (manyTill anyChar eof)

-- |Parse the log type
logType :: Parser MessageType
logType = Info             <$  (char 'I')
      <|> Warning          <$  (char 'W')
      <|> Error <$> (read) <$> (char 'E' *> many1 space *> many1 digit)

-- |Parse message timestamp
timeStamp :: Parser TimeStamp
timeStamp = read <$> many1 digit
