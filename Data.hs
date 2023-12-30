module Data where

data Aexp
  = Num Integer
  | Var String
  | AddAexp Aexp Aexp
  | MultAexp Aexp Aexp
  | SubAexp Aexp Aexp
  deriving (Show)

data Bexp
  = TrueBexp
  | FalseBexp
  | NotBexp Bexp
  | EquBexp Aexp Aexp
  | EqBexp Bexp Bexp
  | LeBexp Aexp Aexp
  | AndBexp Bexp Bexp
  deriving (Show)

data Stm
  = AssStm String Aexp
  | IfStm Bexp [Stm] [Stm]
  | WhileStm Bexp [Stm]
  deriving (Show)