module Cis194.Spring13.HW03.Golf
    ( skips
) where

-- |The output of skips is a list of lists. The first list in the output should
-- |be the same as the input list. The second list in the output should contain
-- |every second element from the input list... and the nth list in the output
-- |should contain every nth element from the input list.
skips :: [a] -> [[a]]
skips = f 1
    where f n l@(x:xs) = (x : e) : (f (n+1) xs)
              where e = [l !! y | y <- [1..(length l)-1], y `mod` n == 0]
          f _ [] = []
