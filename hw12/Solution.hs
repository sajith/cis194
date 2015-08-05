
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

-- TODO: I'm confused by the problem description... where do one
-- "simulate randomly rolling the appropriate number of dice" and how?
battle :: Battlefield -> Rand StdGen Battlefield
battle field = do
    let (as, ds) = dieRoll field
        remAs    = attackers field - as
        remDs    = defenders field - ds
    return $ Battlefield remAs remDs

------------------------------------------------------------

-- Repeated calls to battle until there are defenders remaining, or
-- fewer than two attackers.
invade :: Battlefield -> Rand StdGen Battlefield
invade field = undefined

------------------------------------------------------------
