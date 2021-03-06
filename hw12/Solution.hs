
module Solution where

import           Control.Monad        (replicateM)
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
invade field@(Battlefield att def) = if def > 0 || att > 2
               then do
                    field' <- battle field
                    invade field'
               else return field

------------------------------------------------------------

defLost :: Battlefield -> Bool
defLost (Battlefield a d) = d == 0 && a > 2

defLosses :: [Battlefield] -> Double
defLosses = sum . map (\bf -> if defLost bf then 1 else 0)

successProb :: Battlefield -> Rand StdGen Double
successProb field = do
    invs <- replicateM 1000 (invade field)
    return $ defLosses invs / 1000

------------------------------------------------------------

-- TODO: how to do this?
exactSuccessProb :: Battlefield -> Double
exactSuccessProb field = undefined

------------------------------------------------------------
