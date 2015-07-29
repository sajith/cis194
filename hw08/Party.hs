
module Party where

import           Data.Monoid
import           Data.Tree
import           Employee

import           Data.List   (intercalate, sort)

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
treeFold fn (Node a ns) = fn a (map (treeFold fn) ns)

------------------------------------------------------------

-- TODO: problem statement parse failure.
combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs e gs = undefined

------------------------------------------------------------

-- TODO: problem statement parse failure.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gs = (withBoss, withoutBoss)
    where
        withBoss    = glCons e $ mconcat $ snd $ unzip gs
        withoutBoss = mconcat $ map (uncurry moreFun) gs

------------------------------------------------------------

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

------------------------------------------------------------

main :: IO ()
main = do
    file <- readFile "company.txt"
    let guestList = makeGuestList file
    putStr $ prettify $ maxFun guestList

makeGuestList :: String -> Tree Employee
makeGuestList = read

prettify :: GuestList -> String
prettify (GL es f) = "Total party fun: " ++ show f ++ "\n" ++
                     "People:\n" ++
                     intercalate "\n" (sort (map (\e -> empName e) es))

------------------------------------------------------------
