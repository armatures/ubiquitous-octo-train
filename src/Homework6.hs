module Homework6 where
import Data.List(intersperse)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map ithFib [0..]

ithFib::Int -> Integer
ithFib 0 = 0
ithFib 1 = 1
ithFib x = (fibs2!!(x-1)) + (fibs2!!(x-2))

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = concat . (Data.List.intersperse ", ") . (map show) . (take 20) . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:streamToList xs

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed' f x)
  where streamFromSeed' f' x' = Cons (f' x') (streamFromSeed' f' (f' x'))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (ruler' 1)
  where ruler' x = interleaveStreams (streamRepeat x) (ruler' (x+1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

