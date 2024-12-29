-- Exercise 5

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src dst tmp
  | n <= 0 = []
  | otherwise = hanoi (n - 1) src tmp dst ++ [(src, dst)] ++ hanoi (n - 1) tmp dst src
