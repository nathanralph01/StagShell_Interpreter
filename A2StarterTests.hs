{-|
Module: A2StarterTests
Description: Starter Tests for A2
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2021
-}

module A2StartTest

where

import Test.QuickCheck (Property, (==>), label, quickCheck)

import A2 (runStag, eval)
import A2Types(Expr(..), Value(..), Env)
import qualified Data.Map (lookup, insert, empty)
-- Equal --
-- Test Case --
{-|
runStag (Equal (Literal T) (Literal F))
F
runStag (Equal (Literal F) (Literal F))
T
runStag (Equal (Literal $ Num 3) (Literal $ Num 4)) 
F
runStag (Equal (Plus (Literal $ Num 3) (Literal $ Num 4)) (Literal $ Num 7))
T
runStag (Equal (Literal (Pair T T)) (Literal (Pair F F)))
F
runStag (Equal (Literal (Pair T F)) (Literal (Pair T F)))
T
runStag (Equal (Literal (Pair (Pair T T) F)) (Literal (Pair (Pair T F) (Num 5))))
F
runStag (Equal (Literal (Pair (Pair T F) (Num 5))) (Literal (Pair (Pair T F) (Num 5))))
T
-- Cons --
*A2> runStag (Cons (Plus(Literal $ Num 5)(Literal $ Num 10)) (Plus(Literal $ Num 5)(Literal T)))
Error "Plus"
*A2> runStag (Cons (Plus(Literal $ Num 5)(Literal $ Num 10)) (Plus(Literal $ Num 5)(Literal $ Num 5)))
Pair (Num 15) (Num 10)

-- First --
runStag (First (Cons (Literal $ T) (Plus (Literal $ Num 3) (Literal $ Num 4)) ))
T
runStag (First (Cons (Literal $ T) (Plus (Literal $ T) (Literal $ Num 4))))    
Error "Plus"

-- Function App
runStag (App (Lambda ["x", "y"] (Equal (Var "x") (Var "y"))) [Literal T, Literal T]) 
T
runStag (App (Lambda ["x", "y"] (Equal (Var "x") (Var "y"))) [Literal T, Literal F])
F

-- if --
runStag (If (Literal F) (Times (Literal T)(Literal $ Num 3)) (Literal $ Num 2233))                    
Num 2233
-}

--Some simple tests to get you started--

prop_testLiteralNumber:: Int -> Property
prop_testLiteralNumber x = label "literal numbers" $
    let expr = (Literal $ Num x)
        result = runStag expr
    in result == Num x

prop_testAddition:: Int -> Int -> Property
prop_testAddition x y = label "addition tests" $
    let expr = (Plus (Literal $ Num x) (Literal $ Num y))
        result = runStag expr
    in result == Num (x + y)

prop_testProduct:: Int -> Int -> Property
prop_testProduct x y = label "Multiply tests" $
    let expr = (Times (Literal $ Num x) (Literal $ Num y))
        result = runStag expr
    in result == Num (x * y)

prop_testEqualNum :: Int -> Int -> Property
prop_testEqualNum x y = label "Num Equal tests" $
    let expr = (Equal (Literal $ Num x) (Literal $ Num y))
        result = runStag expr
    in result == if x == y then T else F

prop_testBasicIdentifier :: Property
prop_testBasicIdentifier = label "identifier error" $
  let expr = (Plus (Literal $ Num 3) (Times (Literal $ Num 3) (Literal T)))
      result = runStag expr
  in result == Error "Times"

prop_testFunctionApplication :: Int -> Int -> Property
prop_testFunctionApplication x y = label "function application" $
    let fnExpr1 = Lambda ["x"] (Plus (Literal (Num 1)) (Var "x"))
        fnExpr2 = Lambda ["a", "b"] (Times (Var "a") (App fnExpr1 [(Var "b")]))
        result = runStag (App fnExpr2 [(Literal (Num x)), (Literal (Num y))])
    in result == Num (x * (y + 1))


-------------------------------------------------------------------------------
-- * Main function (for testing purposes only)
-------------------------------------------------------------------------------

-- This main function is executed when you compile and run this Haskell file.
-- It runs the QuickCheck tests; we'll talk about "do" notation much later in
-- the course, but for now if you want to add your own tests, just define them
-- above, and add a new `quickCheck` line below.
main :: IO ()
main = do
    quickCheck prop_testLiteralNumber
    quickCheck prop_testAddition
    quickCheck prop_testProduct
    quickCheck prop_testEqualNum
    quickCheck prop_testBasicIdentifier
    quickCheck prop_testFunctionApplication
