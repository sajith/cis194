-- example from the lecture.

module MTest where

check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

-- return 7  >>= check >>= halve
-- return 7  >>= halve >>= check
-- return 12 >>= check >>= halve
-- return 12 >>= halve >>= check

