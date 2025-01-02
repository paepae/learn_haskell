module Golf where

-- Exercise 1

skips :: [a] -> [[a]]
skips [] = []
skips (x : xs) = [x : xs] ++ skips xs

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : xs) =
  if b > a && b > c
    then [b] ++ localMaxima (b : c : xs)
    else localMaxima (b : c : xs)
localMaxima _ = []

-- Exercise 3

-- Not implemented yet
-- histogram :: [Integer] -> String
