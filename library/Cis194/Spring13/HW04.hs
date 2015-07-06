-- |CIS 194 Homework 4

module Cis194.Spring13.HW04 where

import Data.List (foldl')

-- |fun1 as given in homework
fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

-- |Wholemeal version of fun1
fun1' :: [Integer] -> Integer
fun1' = foldl' (*) 1 . fmap ((-) 2) . filter even

-- |fun2 as given in homework
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- |Wholemeal version of fun2
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1)
      . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- |Geneirc binary tree datatype.
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- |Generates a balanced binary tree from a list of values using foldr.
foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

-- |Insert value into a tree
insertTree :: a -> Tree a -> Tree a
insertTree x (Leaf) = Node 0 Leaf x Leaf
insertTree x (Node ) = Node 0 Leaf x Leaf
