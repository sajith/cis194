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
    add :: a -> a -> a
    mul :: a -> a -> a

------------------------------------------------------------

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

------------------------------------------------------------

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

------------------------------------------------------------

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

------------------------------------------------------------
