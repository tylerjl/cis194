{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Editor
import Data.Monoid
import Buffer
import Sized
import Scrabble

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
aTree :: JoinList Size Char
aTree = Append (Size 4)
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
    where leftSize = jLSize jl1

jLSize :: (Monoid m, Sized m) => JoinList m a -> Int
jLSize = getSize . size . tag

-- .2
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ jl@Empty            = jl
dropJ i jl | i <= 0         = jl
           | i >= jLSize jl = Empty
dropJ _ (Single _ _)        = Empty
dropJ i (Append _ jll jlr)
    | i <  leftSize         = dropJ i jll +++ jlr
    | otherwise             = dropJ (i-leftSize) jlr
    where leftSize = jLSize jll

-- .3
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ (Empty)            = Empty
takeJ i jl | i <= 0        = Empty
           | i > jLSize jl = jl
takeJ _ jl@(Single _ _)    = jl
takeJ i (Append _ left right)
    | i >= leftSize        = left +++ takeJ (i-leftSize) right
    | otherwise            = takeJ i left
    where leftSize = jLSize left

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where

  toString = unlines . jlToList

  fromString = foldr1 (+++)
               . map (\l -> Single (scoreString l, Size 1) l)
               . lines

  line = indexJ

  replaceLine i _ jl | i < 0 || i > jLSize jl = jl
  replaceLine _ l (Empty)      = Single (scoreString l, Size 1) l
  replaceLine _ l (Single _ _) = Single (scoreString l, Size 1) l
  replaceLine i l (Append _ jll jlr)
      | i <  leftSize         = replaceLine i l jll +++ jlr
      | otherwise             = jll +++ replaceLine (i-leftSize) l jlr
      where leftSize = jLSize jll

  numLines = jLSize

  value = f . fst . tag
      where f (Score s) = s

main :: IO ()
main = runEditor editor (fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JoinList (Score, Size) String)
