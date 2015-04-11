{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Monoid
import Data.Tree
import Employee

-- Exercise 1

-- .1
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp {empFun = f}) (GL [] _)  = GL [e] f
glCons e@(Emp {empFun = f}) (GL xs fc) = GL (e:xs) (f+fc)

-- .2
instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL xs f1) (GL ys f2) = GL (xs++ys) (f1+f2)

-- .3
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node {rootLabel = l, subForest = ns}) =
    f l (map (treeFold f) ns)

-- Exercise 3
nextLevel :: Employee ->
             [(GuestList, GuestList)] ->
              (GuestList, GuestList)
nextLevel boss gLists= (bestWith, bestWithout)
    where bestWith    = glCons boss . mconcat . map fst $ gLists
          bestWithout = mconcat . map (uncurry moreFun) $ gLists

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5
main :: IO ()
main = do
     company <- readFile "company.txt"
     putStrLn . printGL $ maxFun (read company :: Tree Employee)

printGL :: GuestList -> String
printGL (GL es fun) = "Total fun: " ++ show fun ++ '\n' : employees
    where employees = unlines . map empName $ es
