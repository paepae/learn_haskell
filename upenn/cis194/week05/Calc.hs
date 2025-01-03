{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

-- Exercise 1

eval :: ExprT -> Integer
eval (Lit n) = n
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr str =
  case parseExpResult of
    Nothing -> Nothing
    Just expr -> Just (eval expr)
  where
    parseExpResult = parseExp Lit ExprT.Add ExprT.Mul str

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit a = (a > 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit a = Mod7 $ a `mod` 7
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: (Expr a) => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer

testBool = testExp :: Maybe Bool

testMM = testExp :: Maybe MinMax

testSat = testExp :: Maybe Mod7

-- Exercise 5
-- Not implemented yet

-- stackVM exp == Right [IVal exp]
-- compile :: String -> Maybe Program

-- Exercise 6
-- Not implemented yet

-- class HasVars where
--   var :: String -> a

-- instance HasVars (M.Map String Integer -> Maybe Integer)
-- instance Expr (M.Map String Integer -> Maybe Integer)

-- withVars :: [(String, Integer)]
-- -> (M.Map String Integer -> Maybe Integer)
-- -> Maybe Integer
-- withVars vs exp = exp $ M.fromList vs
