
module Golf where

import Data.List (group, sort)

------------------------------------------------------------

skips :: [a] -> [[a]]
skips xs = map (`skipn` xs) [1..length xs]
    where
        skipn :: Int -> [a] -> [a]
        skipn n xs' = case drop (n-1) xs' of
            (y:ys) -> y : skipn n ys
            []     -> []

------------------------------------------------------------

-- TODO: this works, but array indexing is bad style.
localMaxima :: [Integer] -> [Integer]
localMaxima xs = concatMap (\n -> [xs !! n | isMaxima n xs]) [1..(length xs-2)]
    where
        isMaxima n xs = (xs !! n) > (xs !! (n-1)) && (xs !! n) > (xs !! (n+1))
             
------------------------------------------------------------

-- TODO: this does not work.
histogram :: [Integer] -> String
histogram xs = makeHistogram xs'
    where
        xs' = group $ sort $ filter (\n -> n >= 0 && n <= 9) xs

        makeHistogram :: [[Integer]] -> String
        makeHistogram xs = show xs

        maxLen xs' = max $ map length xs'

------------------------------------------------------------
