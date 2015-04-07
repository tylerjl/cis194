{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Monoid
import Data.List (foldl')
import Data.Char (toLower)

-- Exercise 3
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score r
    | c `elem` "aeilnorstu" = 1
    | c `elem` "dg"         = 2
    | c `elem` "bcmp"       = 3
    | c `elem` "fhvwy"      = 4
    | c `elem` "k"          = 5
    | c `elem` "jx"         = 8
    | c `elem` "qz"         = 10
    | otherwise             = 0
    where c = toLower r

scoreString :: String -> Score
scoreString = foldl' (<>) (Score 0) . map score
