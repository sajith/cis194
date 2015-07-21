{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Compiler where

import qualified Calc    as C
import qualified StackVM as S

instance C.Expr S.Program where
    lit = undefined
    add = undefined
    mul = undefined

compile :: String -> Maybe S.Program
compile = undefined

