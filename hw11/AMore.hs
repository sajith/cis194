
module AMore where

import Control.Applicative
import Data.Char (isUpper)

import AParser

------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = _

oneOrMore :: Parser a -> Parser [a]
oneOrMore  = _

------------------------------------------------------------
