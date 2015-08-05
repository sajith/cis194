
module Solution where

import           Control.Monad.Random
import           Risk

------------------------------------------------------------

dieRoll :: Battlefield -> (Army, Army)
dieRoll field | att <= 1  = (0, def)
              | def <= 0  = (att, 0)
              | otherwise = (min att 3, min def 2)
    where
        att = attackers field
        def = defenders field


battle :: Battlefield -> Rand StdGen Battlefield
battle field = do
    let (attackers, defenders) = dieRoll field
    deadAttackers <- undefined
    deadDefenders <- undefined
    return field

------------------------------------------------------------
