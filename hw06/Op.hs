{-# LANGUAGE FlexibleContexts  #-}
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
streamMul (Stream x) (Stream y)       = Stream (x * y)

------------------------------------------------------------

instance Num (Stream Integer) where
    (+) = streamOp (+)
    (*) = streamMul

    fromInteger i = Cons i (streamRepeat 0)

    abs    = streamMap abs
    negate = streamMap negate
    signum = streamMap signum

------------------------------------------------------------

{--

-- Currently evaluations look like:

λ> x^4
[0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
λ> (1+x)^5
[1,5,10,10,5,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
λ> (x^2 + x + 3) * (x - 5)
[-15,-2,-4,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
λ>

--}

------------------------------------------------------------

instance Fractional (Stream Integer) where
    (/)          = streamDiv
    recip        = (/) 1     -- TODO: check
    fromRational = undefined -- TODO: later, not necessary here.

streamDiv :: (Num (Stream a), Integral a)
             => Stream a -> Stream a -> Stream a
streamDiv (Cons x xs) (Cons y ys) = q
    where q = Cons (x `div` y) (streamMap (`div` y) (xs - q*ys))

------------------------------------------------------------

-- Whoa cool!
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

------------------------------------------------------------
