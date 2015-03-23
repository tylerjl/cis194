module Fibonacci where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : f fibs2
    where f (x : l@(y:_)) = (x+y) : f l

-- Exercise 3
data Stream a = Cons a (Stream a)

-- instance Show a => Show (Stream a) where
--     show = take 10 streamToList

streamToList :: Stream a -> [a]
streamToList (Cons c s) = c : streamToList s
