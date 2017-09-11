import Test.HUnit
import Homework4
import Homework5.Calc
import qualified Homework5.StackVM as StackVM
import Homework5.ExprT
import Homework5.Parser (parseExp)
import Data.Maybe (fromJust)
import Homework6

main :: IO ()
main = do
  count <- runTestTT $ TestList [ ch4ex1Test
    , ch4ex2Test
    , ch4ex3Test
    , ch5Test
    , ch6Test
    ]
  print count
  return ()

ch6Test :: Test
ch6Test = TestLabel "stream tests" $ TestList
        [ TestCase $
          assertEqual "fib2"
          [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181] $ take 20 $ fibs2
        , TestCase $
          assertEqual "streamRepeat example"
          [1,1] $ take 2 $ streamToList $ streamRepeat 1
        , TestCase $
          assertEqual "streamMap example"
          [2,2] $ take 2 $ streamToList $ streamMap (+1) $ streamRepeat 1
        , TestCase $
          assertEqual "streamFromSeed example"
          [1,2,3,4,5] $ take 5 $ streamToList $ streamFromSeed (+1) 1
        , TestCase $
          assertEqual "nats stream"
          [0,1,2,3,4] $ take 5 $ streamToList $ nats
        , TestCase $
          assertEqual "ruler example"
          6 $ (!!63) $ streamToList $ ruler
        , TestCase $
          assertEqual "ruler sequence"
          [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4] $ take 16 $ streamToList $ ruler
        , TestCase $
          assertEqual "fibonacci stream"
          [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181] $ take 20 $ streamToList fibs3
        ]
ch5Test :: Test
ch5Test = TestLabel "homework 5 tests" $ TestList
      [ TestCase $
        assertEqual "eval example"
        20 $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
      , TestCase $
        assertEqual "evalStr example"
        (Just 20) $ evalStr "(2+3) * 4"
      , TestCase $
        assertEqual "evalStr example"
        Nothing $ (evalStr "2+3 * ")
      , TestCase $
        assertEqual "evalStr example"
        (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) $
        ((mul (add (lit 2) (lit 3)) (lit 4))::ExprT)
      , TestCase $
        assertEqual "" (Just (-7)) $ (testExp :: Maybe Integer)
      , TestCase $
        assertEqual "" (Just True) $ (testExp :: Maybe Bool)
      , TestCase $
        assertEqual "" (Just $ MinMax 5) $ (testExp :: Maybe MinMax)
      , TestCase $
        assertEqual "" (Just $ Mod7 0) $ (testExp :: Maybe Mod7)
      , TestCase $ assertEqual "simple stackVM"
        (Just $ [StackVM.PushI 2, StackVM.PushI 1, StackVM.Add]) $ (parseExp lit add mul "2 + 1")
      , TestCase $
        assertEqual "stack VM" ((Right $ StackVM.IVal 2 )) $ (StackVM.stackVM $ fromJust $ compile "1 + 1")
      ]

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"


ch4ex1Test :: Test
ch4ex1Test = TestLabel "homework 4 exercise 1 tests" $ TestList
      [ TestCase $
        assertEqual "fun1 1 through 5"
        0 $ (fun1 [1..5])
      , TestCase $
        assertEqual "fun1 2 evens"
        0 $ (fun1 [2,12])
      , TestCase $
        assertEqual "fun1 3 odds"
        1 $ (fun1 [1,3,5])
      , TestCase $
        assertEqual "fun1 4,4"
        4 $ (fun1 [4,4])
      , TestCase $
        assertEqual "fun2 1"
        0 $ (fun2 1)
      , TestCase $
        assertEqual "fun2 2"
        2 $ (fun2 2)
      , TestCase $
        assertEqual "fun2 3"
        40 $ (fun2 3)
      , TestCase $
        assertEqual "fun2 4"
        6 $ (fun2 4)                                        ]


ch4ex2Test :: Test
ch4ex2Test = TestLabel "homework 4 exercise 2 tests" $ TestList
      [ TestCase $
        assertEqual "foldTree A"
        ( Node 0 Leaf 'A' Leaf ) $ ( foldTree "A" )
      , TestCase $
        assertEqual "foldTree ABC"
        (
          Node 1 (Node 0 Leaf 'A' Leaf) 'B' ( Node 0 Leaf 'C' Leaf )
        ) $ ( foldTree "ABC" )
      , TestCase $
        assertEqual "foldTree ABCD"
        (
          Node 2
            (Node 1 ( Node 0 Leaf 'A' Leaf ) 'B' Leaf)
            'C'
            ( Node 0 Leaf 'D' Leaf )
        ) $ ( foldTree "ABCD" )
      , TestCase $
        assertEqual "foldTree ABCDEFG"
        (
          Node 2
            (Node 1 ( Node 0 Leaf 'A' Leaf ) 'B' ( Node 0 Leaf 'C' Leaf ))
            'D'
            (Node 1 ( Node 0 Leaf 'E' Leaf ) 'F' ( Node 0 Leaf 'G' Leaf ))
        ) $ ( foldTree "ABCDEFG" )
      ]

ch4ex3Test :: Test
ch4ex3Test = TestLabel "homework 4 exercise 3 tests" $ TestList
      [ TestCase $
        assertEqual "xor 1"
        False $ xor [False, True, False, False, True]
      , TestCase $
        assertEqual "xor 2"
        True $ xor [True]
      , TestCase $
        assertEqual "xor 3"
        True $ xor [False, True, False]
      , TestCase $
        assertEqual "xor 4"
        True $ xor [True, True, True, False, False, False]
      , TestCase $
        assertEqual "map' (+1)"
        ( map (+1) [1..5] ) $ ( map' (+1) [1..5] )
      , TestCase $
        assertEqual "map' reverse"
        ( map reverse ["hi", "there", "mate"] ) $ map' reverse ["hi", "there", "mate"]
      ]

