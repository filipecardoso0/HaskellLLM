module Program where

import Assembler
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as Tok

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

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef

integer :: Parser Integer
integer = Token.integer lexer

identifier :: Parser String
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

parseAexp :: Parser Aexp
parseAexp =
  parens parseAexp
    <|> liftM N integer
    <|> liftM V identifier
    <|> liftM2 Add (parseAexp <* symbol "+") parseAexp
    <|> liftM2 Mult (parseAexp <* symbol "*") parseAexp
    <|> liftM2 Sub (parseAexp <* symbol "-") parseAexp
    <|> liftM Neg (symbol "-" *> parseAexp)

parseBexp :: Parser Bexp
parseBexp =
  parens parseBexp
    <|> (symbol "True" >> return T)
    <|> (symbol "False" >> return F)
    <|> liftM Not (symbol "not" *> parseBexp)
    <|> liftM2 And (parseBexp <* symbol "and") parseBexp
    <|> liftM2 Or (parseBexp <* symbol "or") parseBexp
    <|> liftM2 Eq (parseAexp <* symbol "==") parseAexp
    <|> liftM2 Le (parseAexp <* symbol "<=") parseAexp

parseStm :: Parser Stm
parseStm =
  parens parseStm
    <|> liftM Aexp parseAexp
    <|> liftM Bexp parseBexp
    <|> liftM2 Ass (identifier <* symbol "=") parseAexp
    <|> (symbol "skip" >> return Skip)
    <|> liftM2 Comp (parseStm <* symbol ";") parseStm
    <|> liftM3 If (symbol "if" *> parseBexp <* symbol "then") (semiSep parseStm <* symbol "else") (semiSep parseStm)
    <|> liftM2 While (symbol "while" *> parseBexp <* symbol "do") (semiSep parseStm)
