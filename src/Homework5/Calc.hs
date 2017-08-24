module Homework5.Calc where

import Homework5.ExprT
import Homework5.Parser

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  mul = Mul
  add = Add

instance Expr Integer where
  lit a = a
  mul a b = a * b
  add a b = a + b

instance Expr Bool where
  lit n | n > 0 = True
        | otherwise = False
  mul = (&&)
  add = (||)

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

