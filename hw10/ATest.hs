
module ATest where

import           Control.Applicative
import           Control.Monad

import           AParser

------------------------------------------------------------

conv :: (a -> b) -> (a, c) -> (b, c)
conv f (x, y) = (f x, y)

instance Functor Parser where
    fmap f (Parser p) = Parser (\s -> fmap (conv f) (p s))

-- instance Applicative Parser where
--     pure = pure
--     (<*>) = ap

-- instance Monad Parser where
--     (>>=) = (>>=)
--     return = return

------------------------------------------------------------
