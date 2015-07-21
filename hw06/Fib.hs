module Fib where

fib :: Integer -> Integer
fib n | n < 0     = error "failed"
      | n == 0    = 0
      | n == 1    = 1
      | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

fibs2 :: [Integer]
fibs2 = map fastfib [0..]
    where
        fastfib n = xs !! n
        xs        = map fib' [0..]
        fib'    0 = 0
        fib'    1 = 1
        fib'    n = fastfib (n-1) + fastfib (n-2)

-- failed solutions below.

-- fastfib :: Int -> Integer
-- fastfib 0 = 1
-- fastfib 1 = 1
-- fastfib n | n >= 2  = (xs !! (n-1)) + (xs !! (n-2))
--     where
--         xs = 0 : 1 : map fastfib [2..]

-- fastfib :: Int -> Integer
-- fastfib n = map fib' [0..] !! n
--     where
--         fib' 0 = 0
--         fib' 1 = 1
--         fib' n = fastfib (n-1) + fastfib (n-2)
