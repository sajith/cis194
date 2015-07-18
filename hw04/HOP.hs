module HOP where

------------------------------------------------------------

-- fun1 :: [Integer] -> Integer
-- fun1 []     = 1
-- fun1 (x:xs) | even x  = (x-2) * fun1 xs
--             | otherwise = fun1 xs

fun1 :: [Integer] -> Integer
fun1 = product . map (\n -> if even n then n-2 else 1)

-- fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n | even n    = n + fun2 (n `div` 2)
--        | otherwise = fun2 (3 * n + 1)

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (>1) . iterate fn
    where
        fn n = if even n then n `div` 2 else 3 * n + 1

------------------------------------------------------------

-- TODO: I'm lost.

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Eq, Show)

foldTree :: [a] -> Tree a
foldTree = undefined

insert :: Tree a -> a -> Tree a
insert = undefined

------------------------------------------------------------

