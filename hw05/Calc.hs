{-# LANGUAGE FlexibleInstances #-}

module Calc where

import           ExprT               (ExprT (..))
import           Parser              (parseExp)

import           Control.Applicative ((<$>))

import qualified Data.Map            as M

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

-- TODO: what's a better way to write this?
moper :: (Num a) => (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
moper op (Just x) (Just y) = Just (x `op` y)
moper _  Nothing _         = Nothing
moper _  _       Nothing   = Nothing

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit = const . Just
    add e1 e2 m = moper (+) (e1 m) (e2 m)
    mul e1 e2 m = moper (*) (e1 m) (e2 m)

------------------------------------------------------------

-- Testing function, as given.

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

-- withVars [("x", 6)] $ add (lit 3) (var "x") == Just 9
-- withVars [("x", 6)] $ add (lit 3) (var "y") == Nothing
-- withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x")) == Just 54

------------------------------------------------------------
