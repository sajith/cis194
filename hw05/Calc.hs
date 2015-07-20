module Calc where

import           ExprT
import           Parser

import           Control.Applicative ((<$>))

------------------------------------------------------------

eval :: ExprT -> Integer
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

------------------------------------------------------------

evalStr :: String -> Maybe Integer
evalStr str = eval <$> parseExp Lit Add Mul str

------------------------------------------------------------
