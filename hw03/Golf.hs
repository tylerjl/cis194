module Golf where

import Data.List (sort)

skips :: [a] -> [[a]]
skips = f 1
    where f n l@(x:xs) = (x : e) : (f (n+1) xs)
              where e = [l !! x | x <- [1..(length l)-1], x `mod` n == 0]
          f _ [] = []

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
    | x < y && y > z = [y] ++ localMaxima (y:z:zs)
    | otherwise = localMaxima (y:z:zs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram x = vis x ++ "\n" ++ "==========\n" ++ "0123456789\n"
    where vis x = "foo"
