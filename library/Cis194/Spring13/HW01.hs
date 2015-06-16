module Cis194.Spring13.HW01
    ( toDigits
    , toDigitsRev
    , doubleEveryOther
) where

import Data.Monoid

toDigits :: Integer -> [Integer]
toDigits i | i <= 0    = []
           | otherwise = toDigits (i `quot` 10) <> [i `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther (x:y:ys) =
