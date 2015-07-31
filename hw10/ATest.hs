
module ATest where

import           Control.Applicative
import           Control.Monad       (mplus)
import           Data.Char           (isUpper)

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

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser a <|> Parser b = Parser $ \s -> mplus (b s) (a s)

------------------------------------------------------------

intOrUpperCase :: Parser ()
intOrUpperCase = (const () <$> satisfy (isUpper)) <|> (const () <$> posInt)

-- tests:
-- runParser intOrUpperCase "342abcd" == Just ((), "abcd")
-- runParser intOrUpperCase "XYZ"     == Just ((), "YZ")
-- runParser intOrUpperCase "foo"     == Nothing

------------------------------------------------------------
