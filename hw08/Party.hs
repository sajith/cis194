
module Party where

import           Employee

------------------------------------------------------------

glCons :: Employee -> GuestList -> GuestList
glCons e (GL gs fun) = GL (e:gs) (empFun e + fun)

------------------------------------------------------------
