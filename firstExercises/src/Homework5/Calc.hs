{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Homework5.Calc where

import Homework5.ExprT
import Homework5.Parser
import qualified Homework5.StackVM as StackVM

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  mul = Homework5.ExprT.Mul
  add = Homework5.ExprT.Add

instance Expr Integer where
  lit a = a
  mul a b = a * b
  add a b = a + b

instance Expr Bool where
  lit n | n > 0 = True
        | otherwise = False
  mul = (&&)
  add = (||)

instance Expr StackVM.Program where
  lit n = [StackVM.PushI n]
  mul n m = n ++ m ++ [StackVM.Mul]
  add n m = n ++ m ++ [StackVM.Add]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

newtype MinMax  = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit n = MinMax n
  mul (MinMax n) (MinMax m) = MinMax $ min n m
  add (MinMax n) (MinMax m) = MinMax $ max n m

newtype Mod7    = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit n = Mod7 $ n `mod` 7
  mul (Mod7 n)(Mod7 m) = Mod7 $ (n * m) `mod` 7
  add (Mod7 n)(Mod7 m) = Mod7 $ (n + m) `mod` 7

eval :: ExprT -> Integer
eval e = case e of
  Lit n -> n
  Mul a b -> (eval a) * (eval b)
  Add a b -> (eval a) + (eval b)

evalStr :: String -> Maybe Integer
evalStr s= eval <$> parseExp Lit Add Mul s

reify :: ExprT -> ExprT
reify = id

