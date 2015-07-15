
module Golf where

skips :: [a] -> [[a]]
skips xs = [xs] ++ map (\n -> skip n xs) [1..length xs-1]

skip :: Int -> [a] -> [a]
skip n [] = []
skip n xs = (take n $ drop n xs) ++ skip n (drop (2*n) xs)

