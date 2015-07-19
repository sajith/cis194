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
foldTree = foldr add Leaf

add :: a -> Tree a -> Tree a
add a Leaf                  = Node 0 Leaf a Leaf
add a (Node h Leaf a' Leaf) = Node (h+1) (Node 0 Leaf a Leaf) a' Leaf
add a (Node h Leaf a' r)    = Node h (Node 0 Leaf a Leaf) a' r
add a n@(Node h l a' r)     = if height l == h - 1
                              then Node (newHeight n) l a' (add a r)
                              else Node (newHeight n) (add a l) a' r

height :: Tree a -> Integer
height Leaf           = 0
height (Node h _ _ _) = h

newHeight :: Tree a -> Integer
newHeight (Node _ l _ r) = if height l > height r
                           then height l + 1
                           else height r + 1
newHeight Leaf           = error "something terrible happened"

------------------------------------------------------------

xor :: [Bool] -> Bool
xor = foldl (/=) False

------------------------------------------------------------

map' :: (a -> b) -> [a] -> [b]
map' fn = foldr ((:) . fn) []

------------------------------------------------------------

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl fn = foldr (flip fn)

------------------------------------------------------------

-- Before reading wikipedia.

sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = filter odd $ primes (2*n+2)
    where
        primes :: Integer -> [Integer]
        primes n = filter isPrime [2..n]

        isPrime :: Integer -> Bool
        isPrime n = all (\m -> n `mod` m /= 0) [2..n `div` 2]

------------------------------------------------------------

-- After reading wikipedia

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\p -> 2 * p + 1) (focusList n)

focusList :: Integer -> [Integer]
focusList n = filter (`sieve` n) (range n)

range :: Integer -> [Integer]
range n = [1..n]

sieve :: Integer -> Integer -> Bool
sieve m n = notElem m $ eliminationList n

eliminationList :: Integer -> [Integer]
eliminationList n = map (\(i, j) -> i + j + 2*i*j) $ pairs n

pairs :: Integer -> [(Integer, Integer)]
pairs n = [(i,j) | i <- [1..n],
                   j <- [1..n],
                   i + j + 2*i*j <= n,
                   1 <= i,
                   1 <= j,
                   i <= j ]

------------------------------------------------------------
