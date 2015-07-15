
module Golf where

-- TODO: fix this
skips :: [a] -> [[a]]
skips xs = map (`skipn` xs) [1..length xs-1]
    where
        skipn :: Int -> [a] -> [a]
        skipn n xs' = case drop (n-1) xs' of
            (y:ys) -> y : skipn n ys
            []     -> []

