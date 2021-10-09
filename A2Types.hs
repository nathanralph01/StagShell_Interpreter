{-|
Module: A2Types
Description: A2Types
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2021
-}
module A2Types
    ( Expr(..), Value(..), Env
    )
where

import Data.Map

-- | An environment is a mapping of names to values
type Env =  Map String Value


data Expr = Literal Value           -- literal values
          | Plus Expr Expr          -- builtin "plus" function
          | Times Expr Expr         -- builtin "times" function
          | Equal Expr Expr         -- builtin checks for equality
          | Cons Expr Expr          -- builtin "cons" function that creates a pair
          | First Expr              -- builtin "first" function that obtains the first element of a pair
          | Rest Expr               -- builtin "rest" function that obtains the second element of a pair
          | Var String              -- variable names
          | If Expr Expr Expr       -- if expressions
          | Lambda [String] Expr    -- function definitions
          | App Expr [Expr]         -- function applications
          deriving (Eq, Show) -- this line is so that values of this type
                              -- can be printed, and compared with "=="

data Value = T | F                  -- booleans true an dfalse
           | Num Int                -- integers
           | Pair Value Value       -- pairs
           | Closure [String] Env Expr -- closures
           | Error String           -- errors
           deriving (Eq, Show) -- this line is so that values of this type
                               -- can be printed, and compared with "=="


