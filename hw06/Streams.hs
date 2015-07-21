
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
