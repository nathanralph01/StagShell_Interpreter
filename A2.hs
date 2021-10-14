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
  (T, F) -> F
  (F, T) -> F
  (F, F) -> T
  (T, T) -> T
  (Num x, Num y) -> if x == y then T else F

  (Pair x1 x2, Pair y1 y2) -> if (x1 == y1 && x2 == y2) then T else F
  _              -> Error "idfk"
eval env (Var name)  = case (Data.Map.lookup name env) of
    Just a  -> undefined -- "a" is of type Value 
    Nothing -> Error "Not in the scope" -- "name" is not found in "env"
-- todo: handle Lambda and App
eval env _           = undefined -- todo



