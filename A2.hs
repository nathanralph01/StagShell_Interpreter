{-|
Module: A2
Description: Assignment 2
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2021
-}
-- This lists what this module exports. Don't change this!
module A2
  (
    runStag,
    eval
  )
where

-- You *may not* add imports from Data.Map, or any other imports
import A2Types(Expr(..), Value(..), Env)
import qualified Data.Map (lookup, insert, empty)


-- | Runs a StagShell expression by calling `eval` with the empty environment
runStag :: Expr -> Value
runStag e = eval Data.Map.empty e


-- | An interpreter for the StagShell language.
eval :: Env -> Expr -> Value
eval env (Literal v) = v
-- Plus
eval env (Plus a b)  = case ((eval env a), (eval env b)) of
  (Error x, y)        -> Error x
  (x, Error y)        -> Error y
  (Num x, Num y) -> Num (x + y) -- Numbers -> Add together
  _              -> Error "Plus" -- Not number -> reutrn error?
  -- what other patterns are missing above?
-- Times
eval env (Times a b) = case ((eval env a), (eval env b)) of
  (Error x, y)        -> Error x
  (x, Error y)        -> Error y
  (Num x, Num y) -> Num (x*y) -- Numbers -> Multiply together
  _              -> Error "Times" -- Not Num -> Return Error

-- Equal
eval env (Equal a b) = case ((eval env a), (eval env b)) of 
  (Error x, y)        -> Error x
  (x, Error y)        -> Error y
  (x, y)              -> if x == y then T else F

-- Cons
eval env (Cons a b) = case ((eval env a), (eval env b)) of 
  (Error x, y)        -> Error x
  (x, Error y)        -> Error y
  (x, y)              -> Pair x y

-- First
eval env (First a) = case ((eval env a)) of
  Error x       -> Error x
  (Pair x y)    -> x
  (_)           -> Error "First"

-- Rest
eval env (Rest a) = case ((eval env a)) of
  Error x       -> Error x
  (Pair x y)    -> y
  (_)           -> Error "Rest"

-- If
eval env (If a b c) = case ((eval env a), (eval env b), (eval env c)) of
  (Error x, _, _)     -> Error x
  (T, y, z)           -> y
  (_, y, z)           -> z

-- Variable lookup
eval env (Var name)  = case (Data.Map.lookup name env) of
    Just a  -> a -- "a" is of type Value 
    Nothing -> Error "Var" -- "name" is not found in "env"
-- todo: handle Lambda and App

-- Function Expression
eval env (Lambda lst body) = case (lst, body) of
  (lst, body)         -> Closure lst env body

-- Function Application
eval env (App fnExpr argExprs) = case ((eval env fnExpr), argExprs) of
  ((Closure [] cenv body), [])          -> (eval cenv body)
  ((Closure (p:ps) cenv body), (x:xs))  -> if (length (p:ps)) /= (length (x:xs))
                                                then Error "App"
                                              else
                                                (eval (Data.Map.insert p (eval cenv x) cenv) (App (Lambda ps body) xs))
                                                --(Data.Map.insert p (eval cenv x) cenv)
  (_, argsExpr)       -> Error "App"


-- eval env _           = undefined -- todo
