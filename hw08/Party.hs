
module Party where

import           Data.Monoid
import           Data.Tree
import           Employee

------------------------------------------------------------

glCons :: Employee -> GuestList -> GuestList
glCons e (GL gs fun) = GL (e:gs) (empFun e + fun)

------------------------------------------------------------

instance Monoid GuestList where
    mempty                        = GL [] 0
    mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

------------------------------------------------------------

moreFun :: GuestList -> GuestList -> GuestList
moreFun a@(GL ls1 f1) b@(GL ls2 f2) = if f1 > f2 then a else b

------------------------------------------------------------

-- TODO: Recheck.  Not sure if this is correct.
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold fn (Node a ns) = f a (map (treeFold fn) ns)

------------------------------------------------------------
