
module JoinList where

import           Data.Monoid
import           Sized

------------------------------------------------------------

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

------------------------------------------------------------

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty xs = xs
(+++) xs Empty = xs
(+++) xs ys    = Append (mappend (tag xs) (tag ys)) xs ys

------------------------------------------------------------

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

------------------------------------------------------------

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _  Empty           = Nothing
indexJ 0 (Single _ a)     = Just a
indexJ _ (Single _ _)     = Nothing
indexJ i (Append _ xs ys) = if i < n
                            then indexJ i xs
                            else indexJ (i - n) ys
    where
        n :: Int
        n = len xs

        len :: (Sized b, Monoid b) => JoinList b a -> Int
        len xs = subl xs 0

        subl :: (Sized b, Monoid b) => JoinList b a -> Int -> Int
        subl Empty            acc = acc
        subl (Single _ _)     acc = acc + 1
        subl (Append _ xs ys) acc = acc + subl xs 0 + subl ys 0

------------------------------------------------------------
