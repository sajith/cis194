
module JoinList where

import           Data.Monoid

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
