{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import           Data.Monoid
import           Scrabble
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
        n = lengthJ xs

lengthJ :: (Sized b, Monoid b) => JoinList b a -> Int
lengthJ xs = subl xs 0
    where
        subl :: (Sized b, Monoid b) => JoinList b a -> Int -> Int
        subl Empty            acc = acc
        subl (Single _ _)     acc = acc + 1
        subl (Append _ xs ys) acc = acc + subl xs 0 + subl ys 0

-- TODO: Without this dummy instance, compiler complains when trying
-- to use indexJ on 'Product' structures.  (But this doesn't seem to
-- be actually used!)  I don't understand why.
instance Sized (Product a0) where
    size = undefined

------------------------------------------------------------

-- To test indexJ, these functions are given:

(!!?) :: [a] -> Int -> Maybe a
(!!?) [] _         = Nothing
(!!?) _  i | i < 0 = Nothing
(!!?) (x:_) 0      = Just x
(!!?) (_:xs) i     = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ xs ys) = jlToList xs ++ jlToList ys

------------------------------------------------------------

-- drop first n elements from xs
-- TODO: this is wrong; this drops just 1 item for n > 1 & xs == Append m a b
dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ 0 xs                     = xs
dropJ n xs | n < 0             = xs
dropJ _ Empty                  = Empty
dropJ n (Single _ _)           = Empty
dropJ _ (Append _ Empty Empty) = Empty
dropJ n (Append m Empty ys)    = Append m Empty (dropJ n ys)
dropJ n (Append m xs ys)       = Append m (dropJ n xs) ys

------------------------------------------------------------

-- TODO: this also is wrong, just the way dropJ is wrong...
takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0       = Empty
takeJ _ Empty            = Empty
takeJ 1 (Single b a)     = Single b a
takeJ n (Append m xs ys) | n <= xsl       = takeJ n xs
                         | n >= xsl + ysl = Append m xs ys
                         | otherwise      = Append m
                                            (takeJ xsl xs)
                                            (takeJ (n - xsl) ys)
    where
        xsl = lengthJ xs
        ysl = lengthJ ys

------------------------------------------------------------

-- TODO: this will not work as in the example...
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

------------------------------------------------------------

-- TODO: Do problem 4. Also revisit the other TODO items above.

------------------------------------------------------------
