module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)


-- Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- Exercise 2

-- Example functions
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Test JoinList
yeahTree = Append (Size 4)
               (Append (Size 2)
                   (Single (Size 1) 'a')
                   (Single (Size 1) 'b'))
               (Append (Size 2)
                   (Single (Size 1) 'c')
                   (Single (Size 1) 'd'))

-- .1
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ (Empty)            = Nothing
indexJ i _  | i < 0         = Nothing
indexJ i jl | i > jLSize jl = Nothing
indexJ 0 (Single _ j)       = Just j
indexJ _ (Single _ _)       = Nothing
indexJ i (Append _ jl1 jl2)
    | i >= leftSize = indexJ (i-leftSize) jl2
    | otherwise     = indexJ i jl1
    where leftSize = jLSize jl2

jLSize :: (Monoid m, Sized m) => JoinList m a -> Int
jLSize = getSize . size . tag
