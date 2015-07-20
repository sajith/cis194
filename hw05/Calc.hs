module Calc where

import           ExprT               (ExprT (..))
import           Parser              (parseExp)

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

class Expr a where
    lit :: Integer -> a
    add :: Integer -> Integer -> a
    mul :: Integer -> Integer -> a

------------------------------------------------------------

instance Expr ExprT where
    lit x   = Lit x
    add x y = Add (Lit x) (Lit y)
    mul x y = Mul (Lit x) (Lit y)

------------------------------------------------------------

