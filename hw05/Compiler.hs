{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Compiler where

import qualified Calc    as C
import qualified StackVM as S

instance C.Expr S.Program where
    lit x   = [S.PushI x]
    add x y = x ++ y ++ [S.Add]
    mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = undefined

