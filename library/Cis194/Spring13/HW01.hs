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

-- |Convert an integer into a list of its digits.
toDigits :: Integer -> [Integer]
toDigits i | i <= 0    = []
           | otherwise = toDigits (i `quot` 10) <> [i `mod` 10]

-- |Convert an integer into a reversed list of its digits.
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- |Double every other integer in a list beginning from the right.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1,2]) . reverse

-- |Sum all the integers in a list.
sumDigits :: [Integer] -> Integer
sumDigits = foldl' (+) 0 . (=<<) (toDigits)

{-|
  Validate the integer representation of credit card number.
  Returns a boolean indicating whether the number is valid.
-}
validate :: Integer -> Bool
validate = ((==) 0 . flip mod 10) . sumDigits . doubleEveryOther . toDigits

-- Towers of Hanoi

type Peg  = String
type Move = (Peg, Peg)

-- |Solve a three-peg towers of hanoi puzzle with 'Integer' disks.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi n' a c b <> hanoi n' a b c <> hanoi n' c b a
    where n' = n - 1
