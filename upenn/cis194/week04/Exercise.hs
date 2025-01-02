-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x acc -> (x - 2) * acc) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- Not implemented yet
-- fun2' :: Integer -> Integer

-- Exercise 2
-- Not implemented yet

-- foldTree :: [a] -> Tree a
-- foldTree =

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr (\_ s -> not s) False . filter (== True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

-- Not implemented yet
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr

-- Exercise 4
-- Not implemented yet

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

-- sieveSundaram :: Integer -> [Integer]
-- sieveSundaram =
