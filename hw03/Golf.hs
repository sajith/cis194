
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

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) = if y > x && y > z
                         then y:localMaxima (z:xs)
                         else localMaxima (y:z:xs)
localMaxima _          = []

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
