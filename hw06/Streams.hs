
module Streams where

------------------------------------------------------------

data Stream a = Stream a | Cons a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

------------------------------------------------------------

streamToList :: Stream a -> [a]
streamToList (Stream a)    = [a]
streamToList (Cons a rest) = a : streamToList rest

------------------------------------------------------------

streamRepeat :: a -> Stream a
streamRepeat s = Cons s (streamRepeat s)

------------------------------------------------------------

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a)    = Stream (f a)
streamMap f (Cons a rest) = Cons (f a) (streamMap f rest)

------------------------------------------------------------

-- TODO: is this even correct?
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f v = Cons v (streamFromSeed f (f v))

------------------------------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

------------------------------------------------------------

ruler :: Stream Integer
ruler = streamMap (\m -> pow2 m (m `div` 2)) (streamFromSeed (+1) 1)
    where
        pow2 :: Integer -> Integer -> Integer
        pow2 m n = if m `mod` (2^n) == 0 then n else pow2 m (n-1)

------------------------------------------------------------
