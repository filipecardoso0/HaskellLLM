module Program where

import Assembler
import Data
import Data.Char (isDigit)
import Data.List (elemIndex, isPrefixOf)
import Text.Parsec.Token qualified as Tok

compA :: Aexp -> Code
compA command = case command of
  Num n -> [Push n]
  Var v -> [Fetch v]
  AddAexp a1 a2 -> compA a2 ++ compA a1 ++ [Add]
  MultAexp a1 a2 -> compA a2 ++ compA a1 ++ [Mult]
  SubAexp a1 a2 -> compA a2 ++ compA a1 ++ [Sub]

compB :: Bexp -> Code
compB command = case command of
  TrueBexp -> [Tru]
  FalseBexp -> [Fals]
  NotBexp b -> compB b ++ [Neg]
  EqBexp a1 a2 -> compB a1 ++ compB a2 ++ [Equ]
  EquBexp a1 a2 -> compA a1 ++ compA a2 ++ [Equ]
  LeBexp a1 a2 -> compA a2 ++ compA a1 ++ [Le]
  AndBexp b1 b2 -> compB b1 ++ compB b2 ++ [And]

compile :: [Stm] -> Code
compile [] = []
compile (command : rest) = case command of
  AssStm x a -> compA a ++ [Store x] ++ compile rest
  IfStm x a b -> compB x ++ [Branch (compile a) (compile b)] ++ compile rest
  WhileStm x a -> Loop (compB x) (compile a) : compile rest

lexer :: String -> [String]
lexer [] = []
lexer str
  | "<=" `isPrefixOf` str = "<=" : lexer (drop 2 str)
  | "==" `isPrefixOf` str = "==" : lexer (drop 2 str)
  | ":=" `isPrefixOf` str = ":=" : lexer (drop 2 str)
  | otherwise = case head str of
      ' ' -> lexer (tail str)
      c
        | c `elem` "();=+-*" ->
            [c] : lexer (tail str)
      _ ->
        let (token, rest) = break (`elem` " ();=+-*<:") str
         in token : lexer rest

parseStatements :: [String] -> [Stm]
parseStatements [] = []
parseStatements tokens = case locateOuterTarget [";"] tokens of
  Just index ->
    let (statementTokens, remainingTokens) = splitAt index tokens
     in if head statementTokens == "("
          then parseStatements (tail (init statementTokens))
          else case remainingTokens of
            [_] -> [constructStatement statementTokens]
            _ -> constructStatement statementTokens : parseStatements (tail remainingTokens)
  Nothing -> parseStatements (tail (init tokens))

constructStatement :: [String] -> Stm
constructStatement list =
  case head list of
    "if" ->
      let (bexp, rest) = break (== "then") list
       in case locateOuterTarget ["else"] (tail rest) of
            Just index ->
              let (stm1, stm2) = splitAt index (tail rest)
               in case head (tail stm2) of
                    "(" -> IfStm (constructBooleanExpression (tail bexp)) (parseStatements stm1) (parseStatements (tail stm2))
                    _ -> IfStm (constructBooleanExpression (tail bexp)) (parseStatements stm1) [constructStatement (tail stm2)]
    "while" ->
      let (bexp, stm) = break (== "do") list
       in case head (tail stm) of
            "(" -> WhileStm (constructBooleanExpression (tail bexp)) (parseStatements (tail stm))
            _ -> WhileStm (constructBooleanExpression (tail bexp)) [constructStatement (tail stm)]
    _ ->
      let (var, aexp) = break (== ":=") list
       in AssStm (head var) (constructArithmeticExpression (tail aexp))

locateOuterTarget :: [String] -> [String] -> Maybe Int
locateOuterTarget targets = locate 0 0
  where
    locate _ _ [] = Nothing
    locate depth position (token : remainingTokens) =
      case token of
        "(" -> locate (depth + 1) (position + 1) remainingTokens
        "then" -> locate (depth + 1) (position + 1) remainingTokens
        ")" -> locate (depth - 1) (position + 1) remainingTokens
        "else" | depth /= 0 -> locate (depth - 1) (position + 1) remainingTokens
        _ ->
          if depth == 0 && (token `elem` targets)
            then Just position
            else locate depth (position + 1) remainingTokens

constructArithmeticExpression :: [String] -> Aexp
constructArithmeticExpression [x] = if all isDigit x then Num (read x) else Var x
constructArithmeticExpression tokens =
  case locateOuterTarget ["+", "-"] (reverse tokens) of
    Just reversedIndex ->
      let index = length tokens - reversedIndex - 1
          (before, after) = splitAt index tokens
       in if tokens !! index == "+"
            then AddAexp (constructArithmeticExpression before) (constructArithmeticExpression (tail after))
            else SubAexp (constructArithmeticExpression before) (constructArithmeticExpression (tail after))
    Nothing ->
      case locateOuterTarget ["*"] (reverse tokens) of
        Just reversedIndex ->
          let index = length tokens - reversedIndex - 1
              (before, after) = splitAt index tokens
           in MultAexp (constructArithmeticExpression before) (constructArithmeticExpression (tail after))
        Nothing -> constructArithmeticExpression (tail (init tokens))

constructBooleanExpression :: [String] -> Bexp
constructBooleanExpression [x] =
  case x of
    "True" -> TrueBexp
    "False" -> FalseBexp
    _ -> error "Run-time error"
constructBooleanExpression tokens =
  case locateOuterTarget ["and"] (reverse tokens) of
    Just reversedIndex ->
      let index = length tokens - reversedIndex - 1
          (before, after) = splitAt index tokens
       in AndBexp (constructBooleanExpression before) (constructBooleanExpression (tail after))
    Nothing ->
      case locateOuterTarget ["="] (reverse tokens) of
        Just reversedIndex ->
          let index = length tokens - reversedIndex - 1
              (before, after) = splitAt index tokens
           in EqBexp (constructBooleanExpression before) (constructBooleanExpression (tail after))
        Nothing ->
          case locateOuterTarget ["not"] (reverse tokens) of
            Just reversedIndex ->
              let index = length tokens - reversedIndex - 1
                  after = drop index tokens
               in NotBexp (constructBooleanExpression (tail after))
            Nothing ->
              case locateOuterTarget ["=="] (reverse tokens) of
                Just reversedIndex ->
                  let index = length tokens - reversedIndex - 1
                      (before, after) = splitAt index tokens
                   in EquBexp (constructArithmeticExpression before) (constructArithmeticExpression (tail after))
                Nothing ->
                  case locateOuterTarget ["<="] (reverse tokens) of
                    Just reversedIndex ->
                      let index = length tokens - reversedIndex - 1
                          (before, after) = splitAt index tokens
                       in LeBexp (constructArithmeticExpression before) (constructArithmeticExpression (tail after))
                    Nothing -> constructBooleanExpression (tail (init tokens))

parse :: String -> [Stm]
parse = parseStatements . lexer