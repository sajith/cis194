
module ATest where

import           Control.Applicative
import           Control.Monad

import           AParser

------------------------------------------------------------

conv :: (a -> b) -> (a, c) -> (b, c)
conv f (x, y) = (f x, y)

instance Functor Parser where
    fmap f (Parser p) = Parser (fmap (conv f) . p)

instance Applicative Parser where
    pure a = Parser (\s -> Just (a, s))
    (<*>) (Parser f) b@(Parser p) =
        Parser $ \s ->
            case f s of
                Nothing       -> Nothing
                Just (f', s') -> runParser (fmap f' b) s'

-- instance Monad Parser where
--     (>>=) = undefined
--     return = undefined

------------------------------------------------------------
