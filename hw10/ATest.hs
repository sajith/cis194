
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

data Employee = Employee { name  :: Name
                         , phone :: String }
                deriving (Show)

-- TODO: write this
-- parseName :: Parser Name
-- parseName = Parser n
--     where

-- TODO: write this
parsePhone :: Parser String
parsePhone = Parser phone
    where
        -- phone = and $ map (\p -> p `elem` ['0'..'9'])
        phone = undefined

-- Employee <$> parseName <*> parsePhone

------------------------------------------------------------

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- tests:
-- runParser abParser "abcdef" == Just (('a','b'),"cdef")
-- runParser abParser "aebcdf" == Nothing

------------------------------------------------------------

abParser_ :: Parser ()
abParser_ = const () <$> abParser

-- tests:
-- runParser abParser_ "abcdef" == Just ((),"cdef")
-- runParser abParser_ "aebcdf" == Nothing

------------------------------------------------------------

intPair :: Parser [Integer]
intPair = (\i1 _ i2 -> [i1,i2]) <$> posInt <*> char ' ' <*> posInt

-- tests:
-- runParser intPair "12 34" == Just ([12,34],"")
-- runParser intPair "1234" == Nothing

------------------------------------------------------------

-- class Applicative f => Alternative f where
--     empty :: f a
--     (<|>) :: f a -> f a -> f a

------------------------------------------------------------

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser a <|> Parser b = Parser $ \s -> mplus (b s) (a s)

------------------------------------------------------------
