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
          f (x:xs) = x : f xs
          f [] = []

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons c s) = c : streamToList s

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons c a) = Cons (f c) (streamMap f a)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f $ f a

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (1 +) 0

ruler :: Stream Integer
ruler = streamMap f $ streamTail nats
    where f n | odd n     = 0
              | otherwise = g n $ floor $ logBase 2 $ fromIntegral n
          g item power | item `mod` (2^power) == 0 = power
                       | otherwise                 = g item (power-1)

streamTail :: Stream a -> Stream a
streamTail (Cons _ xs) = xs
