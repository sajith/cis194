
-- Credit Card validation

toDigits :: Integer -> [Integer]
toDigits n = toDigits' n [] where
    toDigits' :: Integer -> [Integer] -> [Integer]
    toDigits' n acc =
        case n `div` 10 of
            0 -> n `mod` 10 : acc
            _ -> toDigits' (n `div` 10) (n `mod` 10 : acc)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (y:x:xs) = y : 2 * x : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concat $ map toDigits xs

validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigitsRev n) `mod` 10 == 0

