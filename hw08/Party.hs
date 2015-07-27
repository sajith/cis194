
module Party where

import           Data.Monoid
import           Employee

------------------------------------------------------------

glCons :: Employee -> GuestList -> GuestList
glCons e (GL gs fun) = GL (e:gs) (empFun e + fun)

------------------------------------------------------------

instance Monoid GuestList where
    mempty                        = GL [] 0
    mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

------------------------------------------------------------
