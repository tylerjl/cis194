module Golf where

import Data.List (transpose)

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
histogram x = (unlines . transpose . map (g (maximum i)) $ i) ++ f
    where i = map (\j -> length . filter (== j) $ x) [0..9]
          g v n = [' ' | _ <- [(n+1)..v]] ++ ['*' | _ <- [1..n]]
          f = "==========\n" ++ ['0'..'9'] ++ "\n"
