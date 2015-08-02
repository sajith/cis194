
module AMore where

import           Control.Applicative
import           Data.Char           (isAlpha, isAlphaNum, isSpace, isUpper)

import           AParser
import           SExpr               hiding (ident, oneOrMore, spaces,
                                      zeroOrMore)

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

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

------------------------------------------------------------

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

-- tests:
-- runParser ident "foobar baz" == Just ("foobar"," baz")
-- runParser ident "foo33fA" == Just ("foo33fA","")
-- runParser ident "2bad" == Nothing
-- runParser ident "" == Nothing

------------------------------------------------------------

parseAtom :: Parser SExpr
parseAtom = undefined

parseExpr :: Parser SExpr
parseExpr = undefined

parseSExpr :: Parser SExpr
parseSExpr = parseAtom <|> parseExpr

-- tests
-- runParser (spaces *> posInt) "   345" == Just (345,"")

------------------------------------------------------------

