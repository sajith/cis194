
module Matrix where

------------------------------------------------------------

data Matrix = Matrix Integer Integer Integer Integer

instance Num (Matrix) where
    (*) = matrixMM
    (+)    = undefined        -- TODO: not necessary here
    abs    = undefined        -- TODO: not necessary here
    signum = undefined        -- TODO: not necessary here
    fromInteger = undefined   -- TODO: not necessary here
    negate = undefined        -- TODO: not necessary here

matrixMM :: Matrix -> Matrix -> Matrix
matrixMM (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22)
    = Matrix (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22)
             (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22)

------------------------------------------------------------

fib4 :: Integer -> Integer
fib4 n = x
    where (Matrix _ x _ _) = (Matrix 1 1 1 0)^n

------------------------------------------------------------
