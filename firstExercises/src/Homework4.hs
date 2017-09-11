module Homework4 where

fun1 :: [Integer] -> Integer
fun1 =  product . (map (\x -> (x - 2))) . (filter even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n  | even n    = n + fun2 (n `div` 2)
        | otherwise = fun2 (3 * n + 1)

-- fun2 = sum . (takeWhile (/= 1)) . iterate (helper)
--         where helper n | even n    = n `div` 2
--                        | otherwise = fun2 (3 * n + 1)

-- fun2 n  | even n    = n + fun2 (n `div` 2)
--         | otherwise = (3 * n + 1)+fun2((3 * n + 1)`div`2)

-- fun2 =  (\n -> n + fun2 (n `div` 2)) . filter even


foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree [x] = Node 0 (foldTree []) x (foldTree [])
foldTree xs = let (head', tail') = splitAt ((length xs)`div`2) xs
                  t1 = foldTree head'
                  t2 = foldTree (tail tail')
                  height' = 1 + max (height t1) (height t2)
                  in
                  Node height' t1 (head tail') t2

height :: Tree a -> Integer
height Leaf = 0
height ( Node i _ _ _ ) = i

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
     deriving (Show, Eq)

xor :: [Bool] -> Bool
xor = let pairwise = (\x y -> (x && not y) || (not x && y))
          in
          foldr pairwise False


map' :: (a -> b) -> [a] -> [b]
-- map' f (x:xs) = (f x) : (map' f xs)
map' f = foldr (\a b -> (f a) : b) []


