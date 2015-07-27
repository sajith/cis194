
module JoinList where

import           Data.Monoid

------------------------------------------------------------

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

------------------------------------------------------------

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) = undefined

------------------------------------------------------------

tag :: Monoid m => JoinList m a -> m
tag Empty          = error "empty"
tag (Single m _)   = m
tag (Append m _ _) = m

------------------------------------------------------------
