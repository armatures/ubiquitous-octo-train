module JoinList where
import Data.Monoid ((<>))
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append ((tag l)<>(tag r)) l r

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m -- mempty <> (tag l) <> (tag r)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ x) = Just x
indexJ i _ | i<0 = Nothing
indexJ i a | not $ containsIndex a i = Nothing
indexJ i (Append _ l r) | not $ containsIndex l i = indexJ (i-(sensibleSize l)) r
                        | otherwise = indexJ i l
indexJ _ (Single _ _) = error "reached leaf without decrementing index to 0"

dropJ :: (Sized b, Monoid b) =>
            Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ i (Append s l r) | not $ containsIndex l i = dropJ (i-sensibleSize l) r
                       | otherwise = Append newSize (newLeft) r
  where newLeft = dropJ i l
        newSize = tag newLeft <> tag r
dropJ _ (Single _ _) = Empty


takeJ :: (Sized b, Monoid b) =>
            Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ count (Single _ _) | count < 0 = error "index is less than zero in takeJ"
takeJ _ x@(Single _ _) = x
-- takeJ i (Append s _ _) | i > (tag s) = error "index is outside the bounds of the joinList"
takeJ count (Append s l r) | (sensibleSize l) >= count = takeJ count l
                       | otherwise = Append newSize l newRight
  where newRight = takeJ (count - sensibleSize l) r
        newSize = tag l <> tag newRight

scoreLine :: String -> JoinList Score String
scoreLine s = foldr scoreLine' Empty (words s)

scoreLine' :: String -> JoinList Score String -> JoinList Score String
scoreLine' word jl = case jl of
  Empty ->
    newNode
  _ ->
    jl +++ newNode
  where newNode = Single (scoreString word) word


containsIndex :: (Sized b, Monoid b) => JoinList b a -> Int -> Bool
containsIndex jl i = i<(sensibleSize jl)

sensibleSize :: (Sized b, Monoid b) => JoinList b a -> Int
sensibleSize = getSize . size . tag
