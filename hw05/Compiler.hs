{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Compiler where

import qualified Calc    as C
import qualified Parser  as P
import qualified StackVM as S

instance C.Expr S.Program where
    lit x   = [S.PushI x]
    add x y = x ++ y ++ [S.Add]
    mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = P.parseExp C.lit C.add C.mul

