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
eval env (Plus a b)  = case ((eval env a), (eval env b)) of
    (Num x, Num y) -> Num (x + y) -- Numbers -> Add together
    _              -> Error "Plus" -- Not number -> reutrn error?
    -- what other patterns are missing above?
eval env (Times a b) = case ((eval env a), (eval env b)) of
  (Num x, Num y) -> Num (x*y) -- Numbers -> Multiply together
  _              -> Error "Times" -- Not Num -> Return Error
-- todo: handle Equal, Cons, First, Rest, and If

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
  (Pair x y)    -> x
  (_)           -> Error "First"

-- Rest
eval env (Rest a) = case ((eval env a)) of
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
    Nothing -> Error "Not in the scope" -- "name" is not found in "env"
-- todo: handle Lambda and App
-- Function Expression

eval env (Lambda (x:xs) body) = case ((x:xs), (eval env body)) of
  _                  -> Error "Test"

eval env _           = undefined -- todo
