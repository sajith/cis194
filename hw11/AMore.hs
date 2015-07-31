
module AMore where

import           Control.Applicative
import           Data.Char           (isAlpha, isUpper)

import           AParser

------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- tests:

-- runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" == Just ("ABC","dEfgH")
-- runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" == Just ("ABC","dEfgH")

-- runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" == Just ("", "abcdeFGh")
-- runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" == Nothing

------------------------------------------------------------
