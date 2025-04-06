{-# LANGUAGE TypeSynonymInstances #-}

module ZeroFive where

import ExprT
import Parser
import StackVM (StackExp(..), Program)

eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
   Just exp -> Just (eval exp)
   Nothing -> Nothing

class Expr a where
   lit :: Integer -> a
   add :: a -> a -> a
   mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add 
  mul = Mul

instance Expr Integer where
  lit a = a
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x = x > 0
  add = (||)
  mul = (&&)

-- newtype constructor to make our default extensible.
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7) 
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

instance Expr Program where
   lit n = [PushI n]
   add a b = a ++ b ++ [Add]
   mul a b = a ++ b ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

