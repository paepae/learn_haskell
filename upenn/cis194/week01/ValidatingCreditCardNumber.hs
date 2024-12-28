-- Exercise 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0 = (n `mod` 10) : toDigitsRev (n `div` 10)
  | otherwise = []

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ls =
  reverse $ doubleEveryOtherFromSecond $ reverse ls
  where
    doubleEveryOtherFromSecond :: [Integer] -> [Integer]
    doubleEveryOtherFromSecond [] = []
    doubleEveryOtherFromSecond [x] = [x]
    doubleEveryOtherFromSecond (x : y : zs) = [x, y * 2] ++ doubleEveryOtherFromSecond zs

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

-- Exercise 4

validate :: Integer -> Bool
validate n
  | n >= 0 = mod (sumDigits . doubleEveryOther . toDigits $ n) 10 == 0
  | otherwise = False
