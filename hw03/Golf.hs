
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

-- TODO: This still doesn't work; add spaces and invert histogram.
histogram :: [Integer] -> String
histogram xs = hist xs ++ "\n==========\n0123456789\n"
    where
        hist xs = intercalate "\n"
            $ transpose [take n $ repeat '*' | n <- times xs]

focus :: [Integer] -> [Integer]
focus xs = filter (\n -> n >= 0 && n <= 9) xs

occur :: Integer -> [Integer] -> Int
occur n xs = length $ filter (== n) xs

times :: [Integer] -> [Int]
times xs = map (\n -> occur n (focus xs)) [0..9]

------------------------------------------------------------
