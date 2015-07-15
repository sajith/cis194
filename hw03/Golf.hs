
module Golf where

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
localMaxima xs = concatMap (\n -> if isMaxima n xs then [xs !! n] else []) [1..(length xs-2)]
    where
        isMaxima n xs = (xs !! n) > (xs !! (n-1)) && (xs !! n) > (xs !! (n+1))
             
------------------------------------------------------------
