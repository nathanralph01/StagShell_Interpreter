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
    (Num x, Num y) -> undefined -- todo
    _              -> undefined -- todo
    -- what other patterns are missing above?
eval env (Times a b) = undefined
-- todo: handle Equal, Cons, First, Rest, and If
eval env (Var name)  = case (Data.Map.lookup name env) of
    Just a  -> undefined -- "a" is of type Value 
    Nothing -> undefined -- "name" is not found in "env"
-- todo: handle Lambda and App
eval env _           = undefined -- todo


