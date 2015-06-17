module Cis194.Spring13.HW01
    ( toDigits
    , toDigitsRev
    , doubleEveryOther
    , sumDigits
    , validate
    , hanoi
) where

import Data.Monoid
import Data.List (foldl')

toDigits :: Integer -> [Integer]
toDigits i | i <= 0    = []
           | otherwise = toDigits (i `quot` 10) <> [i `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1,2]) . reverse

sumDigits :: [Integer] -> Integer
sumDigits = foldl' (+) 0 . (=<<) (toDigits)

validate :: Integer -> Bool
validate = ((==) 0 . flip mod 10) . sumDigits . doubleEveryOther . toDigits

-- Towers of Hanoi

type Peg  = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi n' a c b <> hanoi n' a b c <> hanoi n' c b a
    where n' = n - 1
