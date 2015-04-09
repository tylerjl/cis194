module Party where

import Employee

-- Exercise 1

-- .1
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp {empFun = f}) (GL [] _)  = GL [e] f
glCons e@(Emp {empFun = f}) (GL xs fc) = GL (e:xs) (f+fc)
