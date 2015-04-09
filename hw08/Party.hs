{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Data.Monoid
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
