
module Golf where

import           Data.List (group, intercalate, sort, transpose)

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

histogram :: [Integer] -> String
histogram xs = hist xs ++ "\n==========\n0123456789\n"
    where
        hist xs = intercalate "\n"
                $ transpose
                $ map (\e -> histLine e (maxOccur xs)) $ times xs

        -- filter numbers not in [0..9] out.
        focus :: [Integer] -> [Integer]
        focus = filter (`elem` [0..9])

        -- count occurances of n in xs
        occur :: Integer -> [Integer] -> Int
        occur n xs = length $ filter (== n) xs

        -- make a list of occurances of [0..9]
        times :: [Integer] -> [Int]
        times xs = map (\n -> occur n (focus xs)) [0..9]

        -- find the maximum number of occurances.
        maxOccur :: [Integer] -> Int
        maxOccur xs = maximum $ times xs

        -- form a line in the (horizontal) histogram.
        histLine :: Int -> Int -> String
        histLine rep max = replicate (max - rep) ' ' ++
                           replicate rep '*'

------------------------------------------------------------
