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
-- Equal
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
    quickCheck prop_testBasicIdentifier
    quickCheck prop_testFunctionApplication
