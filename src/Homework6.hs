{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
module Homework6 where
import Data.List(intersperse)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib y = fib (y-1) + fib (y-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map ithFib [0..]

ithFib::Int -> Integer
ithFib 0 = 0
ithFib 1 = 1
ithFib y = (fibs2!!(y-1)) + (fibs2!!(y-2))

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = concat . (Data.List.intersperse ", ") . (map show) . (take 20) . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons y ys) = y:streamToList ys

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons y ys) = Cons (f y) $ streamMap f ys

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f y = Cons y (streamFromSeed' f y)
  where streamFromSeed' f' y' = Cons (f' y') (streamFromSeed' f' (f' y'))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (ruler' 1)
  where ruler' y = interleaveStreams (streamRepeat y) (ruler' (y+1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons z zs) ys = Cons z (interleaveStreams ys zs)

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap (\n -> -n)
  (+) (Cons z zs) (Cons y ys) = Cons (y+z) (ys+zs)
  (*) (Cons a as) (Cons b bs) = Cons (a*b) $ (streamMap (a*) bs) + (as * (Cons b bs))

instance Fractional (Stream Integer) where
  (/) (Cons a as) (Cons b bs) = Cons (div a b) $ streamMap ((div 1 b)*) (as - bs * q)
    where q = (/) (Cons a as) (Cons b bs)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)
