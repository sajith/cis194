
module Solution where

import           Control.Monad.Random
import           Risk

------------------------------------------------------------

dieRoll :: Battlefield -> (Army, Army)
dieRoll field | attackers field <= 1 = (0, defenders field)
              | defenders field <= 0 = (attackers field, 0)
              | otherwise            = (min (attackers field) 3, min (defenders field) 2)


battle :: Battlefield -> Rand StdGen Battlefield
battle field = do
    let (attackers, defenders) = dieRoll field
    deadAttackers <- undefined
    deadDefenders <- undefined
    return field

------------------------------------------------------------
