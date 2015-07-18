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

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Eq, Show)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert a Leaf                  = Node 0 Leaf a Leaf
insert a (Node h Leaf a' Leaf) = Node (h+1) (Node 0 Leaf a Leaf) a' Leaf
insert a (Node h Leaf a' r)    = Node h (Node 0 Leaf a Leaf) a' r
insert a n@(Node h l a' r)     = if height l == h - 1
                                 then Node (newHeight n) l a' (insert a r)
                                 else Node (newHeight n) (insert a l) a' r

height :: Tree a -> Integer
height Leaf           = 0
height (Node h _ _ _) = h

newHeight :: Tree a -> Integer
newHeight (Node _ l _ r) = if (height l > height r)
                           then height l + 1
                           else height r + 1
newHeight Leaf           = error "something terrible happened"

------------------------------------------------------------

