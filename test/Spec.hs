import Test.HUnit
import Homework4
import Homework5.Calc
import Homework5.ExprT
import Homework5.Parser (parseExp)

main :: IO ()
main = do
  runTestTT $ TestList [ ch4ex1Test
    , ch4ex2Test
    , ch4ex3Test
    , ch5ex1Test
    ]
  return ()

ch5ex1Test :: Test
ch5ex1Test = TestLabel "homework 5 exercise 1 tests" $ TestList
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

