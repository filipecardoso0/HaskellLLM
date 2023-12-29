module Program where

import Assembler

data Aexp = N Integer | V String | Add Aexp Aexp | Mult Aexp Aexp | Sub Aexp Aexp | Neg Aexp deriving (Show)

data Bexp = T | F | Not Bexp | And Bexp Bexp | Or Bexp Bexp | Eq Aexp Aexp | Le Aexp Aexp deriving (Show)

data Stm = Aexp Aexp | Bexp Bexp | Ass String Aexp | Skip | Comp Stm Stm | If Bexp [Stm] [Stm] | While Bexp [Stm] deriving (Show)

compA :: Aexp -> Code
compA command = case command of
  N n -> [Push n]
  V v -> [Fetch v]
  Add a1 a2 -> compA a2 ++ compA a1 ++ [Add]
  Mult a1 a2 -> compA a2 ++ compA a1 ++ [Mult]
  Sub a1 a2 -> compA a2 ++ compA a1 ++ [Sub]
  Neg a -> compA a ++ [Neg]

compB :: Bexp -> Code
compB command = case command of
  T -> [Push 1]
  F -> [Push 0]
  Not b -> compB b ++ [Not]
  And b1 b2 -> compB b1 ++ compB b2 ++ [And]
  Or b1 b2 -> compB b1 ++ compB b2 ++ [Or]
  Eq a1 a2 -> compA a1 ++ compA a2 ++ [Eq]
  Le a1 a2 -> compA a2 ++ compA a1 ++ [Le]

compile :: [Stm] -> Code
compile [] = []
compile (command : rest) = case command of
  Aexp a -> compA a ++ compile rest
  Bexp b -> compB b ++ compile rest
  Ass x a -> compA a ++ [Store x] ++ compile rest
  If x a b -> compB x ++ [Branch (compile a) (compile b)] ++ compile rest
  While x a -> Loop (compB x) (compile a) : compile rest
  Skip -> compile rest
  Comp s1 s2 -> compile (s1 : s2 : rest)