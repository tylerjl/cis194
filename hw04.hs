fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

-- Idiomatic implementation
fun1' :: [Integer] -> Integer
fun1' = product . map (flip (-) 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- Idiomatic implementation
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
    where f x = if even x then x `div` 2 else 3 * x + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert e Leaf = Node 0 Leaf e Leaf
insert e n@(Node h l c r)
    | balanced n = Node (h+1) (insert e l) c r
    | balanced l = Node h l c (insert e r)
    | otherwise  = Node h (insert e l) c r

balanced :: Tree a -> Bool
balanced Leaf = True
balanced (Node h l _ r) = let e = (treeHeight l == treeHeight r) in
    e && (balanced l) && (balanced r)
    where treeHeight Leaf = -1
          treeHeight (Node h _ _ _) = h

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldl f False
    where f True  True  = False
          f False True  = True
          f x     False = x

-- Exercise 3.2
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\acc x -> f acc : x) []

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = filter odd $ [3..(2*n)+2]
