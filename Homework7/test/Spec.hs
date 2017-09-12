import Test.HUnit
import Lib
import Data.Monoid

main :: IO ()
main = do
  count <- runTestTT $ TestList [ tests
    ]
  print count
  return ()

tests :: Test
tests = TestLabel "tests" $ TestList
        [ TestCase $
          assertEqual "tag Single"
          2 $ tag (Single (Product 2) 'e')
        , TestCase $
          assertEqual "tag Append"
          6 $ tag (Append (6::Size) (Empty) Empty)
        , TestCase $
          assertEqual "+++"
          6 $ tag ((Single (Product 2) 'e') +++ (Single (Product 3) 'a'))
        , TestCase $
          assertEqual "indexJ and toList 3"
          ((jlToList yeah)!!? 3) $ (indexJ 3 yeah)
        , TestCase $
          assertEqual "indexJ and toList 2"
          ((jlToList yeah) !!? 2) $ (indexJ 2 yeah)
        , TestCase $
          assertEqual "indexJ 2 of yeah"
          (Just 'a') $ (indexJ 2 yeah)
        , TestCase $
          assertEqual "indexJ (-1)"
          Nothing $ (indexJ (-1) yeah)
        , TestCase $
          assertEqual "dropJ a whole list"
          Empty $ (dropJ 4 yeah)
        , TestCase $
          assertEqual "dropJ all but the last element"
          (Single (1) 'h') $ (dropJ 3 yeah)
        , TestCase $
          assertEqual "dropJ more than the size"
          Empty $ (dropJ 5 yeah)
        ]

yeah :: JoinList Size Char
yeah =
  (Single (1) 'y') +++
  (Single (1) 'e') +++
  (Single (1) 'a') +++
  (Single (1) 'h')

-- (indexJ i jl) == (jlToList jl !!? i)

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
(x:xs) !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
