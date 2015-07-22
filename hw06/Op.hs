{-# LANGUAGE FlexibleInstances #-}

module Op where

import           Streams

------------------------------------------------------------

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

------------------------------------------------------------

streamOp :: (t -> t1 -> a) -> Stream t -> Stream t1 -> Stream a
streamOp op (Cons x xs) (Cons y ys) = Cons (x `op` y) (streamOp op xs ys)
streamOp op (Stream x) (Stream y)   = Stream (x `op` y)

streamMul (Cons x xs) yys@(Cons y ys) =
    Cons (x*y) (streamMap (x*) ys + (xs * yys))

------------------------------------------------------------

instance Num (Stream Integer) where
    (+) = streamOp (+)
    (*) = streamMul

    fromInteger i = Cons i (streamRepeat 0)

    abs    = streamMap abs
    negate = streamMap negate
    signum = streamMap signum

------------------------------------------------------------
