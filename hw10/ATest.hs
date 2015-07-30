
module ATest where

import           Control.Applicative
import           Control.Monad

import           AParser

------------------------------------------------------------

conv :: (a -> b) -> (a, c) -> (b, c)
conv f (x, y) = (f x, y)

instance Functor Parser where
    fmap f (Parser p) = Parser (fmap (conv f) . p)

-- TODO: Correct?
instance Applicative Parser where
    pure a = Parser (\s -> Just (a, s))
    (<*>) p1@(Parser f) p2@(Parser p) =
        Parser $ \s ->
            case f s of
                Nothing       -> Nothing
                Just (f', s') -> runParser (fmap f' p2) s'

-- instance Monad Parser where
--     (>>=) = undefined
--     return = undefined

------------------------------------------------------------

-- for testing.

type Name = String

data Employee = Employee { name :: Name
                         , phone :: String }
                deriving (Show)

-- TODO: write this
parseName :: Parser Name
parseName = undefined

-- TODO: write this
parsePhone :: Parser String
parsePhone = undefined

-- Employee <$> parseName <*> parsePhone

------------------------------------------------------------
