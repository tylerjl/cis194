module Golf where

import Data.List (sort, transpose)
import Data.Function (on)

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
histogram x = (unlines . transpose $ (map (vis h s) [0..9])) ++ "\n" ++ f
    where vis height nums column = if column
          f = "==========\n" ++ ['0'..'9'] ++ "\n"
          h = length . maximumBy (compare `on` length) . group . sort $ x
          s = group . sort $ x
