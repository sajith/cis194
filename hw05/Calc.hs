{-# LANGUAGE FlexibleInstances #-}

module Calc where

import           ExprT               (ExprT (..))
import           Parser              (parseExp)

import           Control.Applicative ((<$>))

import           Data.Map            as M

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

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

------------------------------------------------------------

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

------------------------------------------------------------

instance Expr Bool where
    lit n = n > 0
    add   = (||)
    mul   = (&&)

------------------------------------------------------------

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7   = Mod7 Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
    lit = MinMax
    add = min
    mul = max

instance Expr Mod7 where
    lit n                 = Mod7 (n `mod` 7)
    add (Mod7 m) (Mod7 n) = Mod7 ((m + n) `mod` 7)
    mul (Mod7 m) (Mod7 n) = Mod7 ((m * n) `mod` 7)

------------------------------------------------------------

class HasVars a where
    var :: String -> a

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

madd :: Maybe Integer -> Maybe Integer -> Maybe Integer
madd (Just x) (Just y) = Just (x+y)
madd Nothing _         = Nothing
madd _       Nothing   = Nothing

mmul :: Maybe Integer -> Maybe Integer -> Maybe Integer
mmul (Just x) (Just y) = Just (x*y)
mmul Nothing _         = Nothing
mmul _       Nothing   = Nothing

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit = const . Just
    add e1 e2 m = madd (e1 m) (e2 m)
    mul e1 e2 m = mmul (e1 m) (e2 m)

------------------------------------------------------------
