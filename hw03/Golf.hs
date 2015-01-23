module Golf where

import Data.List (sort)

skips :: [a] -> [[a]]
skips x = f 0 x
    where f n l@(y:ys) = (g n l) : (f (n+1) ys)
              where g idx (z:zs) = 
                    g _ [] = []
          f _ [] = []

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
    | x < y && y > z = [y] ++ localMaxima (y:z:zs)
    | otherwise = localMaxima (y:z:zs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram x = vis x ++ "\n" ++ "==========\n" ++ "0123456789\n"
    where vis x = "foo"
