{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as StackVM

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
    Just e  -> Just $ eval e
    Nothing -> Nothing

-- Exercise 3
class Expr e where
    lit :: Integer -> e
    add :: e -> e -> e
    mul :: e -> e -> e

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = flip (<=) 0
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer
    deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer
    deriving (Eq, Show)

instance Expr Mod7 where
    lit i = Mod7 $ i `mod` 7
    add (Mod7 x) (Mod7 y) = Mod7 $ mod 7 $ x + y
    mul (Mod7 x) (Mod7 y) = Mod7 $ mod 7 $ x * y

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

-- Exercise 5
instance Expr StackVM.Program where
    lit i   = [StackVM.PushI i]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile s = case parseExp lit add mul s of
    Nothing -> Nothing
    Just p  -> Just p
