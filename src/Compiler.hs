module Compiler where

import Assembler
import Data
import Data.Char (isDigit)
import Data.List (isPrefixOf)

-- Compiles arithmetic expressions
compA :: Aexp -> Code
compA command = case command of
  Num n -> [Push n]  -- A number compiles into a push command.
  Var v -> [Fetch v]  -- A variable compiles into a fetch command.
  AddAexp a1 a2 -> compA a2 ++ compA a1 ++ [Add]  -- An addition compiles into code for computing the operands, followed by an add command.
  MultAexp a1 a2 -> compA a2 ++ compA a1 ++ [Mult]  -- A multiplication compiles into code for computing the operands, followed by a mult command.
  SubAexp a1 a2 -> compA a2 ++ compA a1 ++ [Sub]  -- A subtraction compiles into code for computing the operands, followed by a sub command.


-- Compiles boolean expressions
compB :: Bexp -> Code
compB command = case command of
  TrueBexp -> [Tru]  -- True compiles into a tru command.
  FalseBexp -> [Fals]  -- False compiles into a fals command.
  NotBexp b -> compB b ++ [Neg]  -- A negation compiles into code for computing the operand, followed by a neg command.
  EqBexp a1 a2 -> compB a1 ++ compB a2 ++ [Equ]  -- An equality compiles into code for computing the operands, followed by an equ command.
  EquBexp a1 a2 -> compA a1 ++ compA a2 ++ [Equ]  -- An equality compiles into code for computing the operands, followed by an equ command.
  LeBexp a1 a2 -> compA a2 ++ compA a1 ++ [Le]  -- A less-than-or-equal-to compiles into code for computing the operands, followed by a le command.
  AndBexp b1 b2 -> compB b1 ++ compB b2 ++ [And]  -- An and compiles into code for computing the operands, followed by an and command.

-- Main compilation function
compile :: Program -> Code
compile [] = [] -- An empty program corresponds to an empty code.
compile (command : rest) = case command of -- For a non-empty program, compile the first command and the rest separately.
  AssStm x a -> compA a ++ [Store x] ++ compile rest -- An assignment statement compiles into code for computing the expression, followed by a store command.
  IfStm x a b -> compB x ++ [Branch (compile a) (compile b)] ++ compile rest -- An if statement compiles into code for evaluating the condition, followed by a branch command.
  WhileStm x a -> Loop (compB x) (compile a) : compile rest -- A while statement compiles into a loop command.

-- Converts a string into a list of tokens
lexer :: String -> [String]
lexer [] = []  -- An empty string corresponds to an empty list of tokens.
lexer str
  | "<=" `isPrefixOf` str = "<=" : lexer (drop 2 str)  -- If the string starts with "<=", "<=" is a token.
  | "==" `isPrefixOf` str = "==" : lexer (drop 2 str)  -- If the string starts with "==", "==" is a token.
  | ":=" `isPrefixOf` str = ":=" : lexer (drop 2 str)  -- If the string starts with ":=", ":=" is a token.
  | otherwise = case head str of  -- If the string starts with something else, check the first character.
      ' ' -> lexer (tail str)  -- If the first character is a space, ignore it.
      c
        | c `elem` "();=+-*" ->  -- If the first character is one of these, it is a token.
            [c] : lexer (tail str)
      _ ->  -- If the first character is something else, the token is the longest prefix not containing these characters.
        let (token, rest) = break (`elem` " ();=+-*<:") str
         in token : lexer rest

-- Parses statements from tokens
parseStm :: [String] -> Program
parseStm [] = []  -- An empty list of tokens corresponds to an empty program.
parseStm tokens = 
  case findTarget [";"] tokens of  -- Try to find a ";" token which separates statements.
    Just index ->  -- If ";" is found, split the tokens into two parts.
      let (statementTokens, remainingTokens) = splitAt index tokens
      in if head statementTokens == "("  -- If the statement starts with "(", remove the parentheses.
          then parseStm (tail (init statementTokens))
          else case remainingTokens of  -- Depending on the remaining tokens, build the statement and continue parsing.
            [_] -> [buildStm statementTokens]
            _ -> buildStm statementTokens : parseStm (tail remainingTokens)
    Nothing -> parseStm (tail (init tokens))  -- If ";" is not found, remove the parentheses and try again.

-- Builds a statement from a list of tokens
buildStm :: [String] -> Stm
buildStm list =
  case head list of
    -- If the first token is "if", we're building an IfStm
    "if" ->
      -- We split the list into a boolean expression and the rest of the tokens
      let (bexp, rest) = break (== "then") list
       in case findTarget ["else"] (tail rest) of
            Just index ->
              -- We split the rest of the tokens into two statements at the "else" token
              let (stm1, stm2) = splitAt index (tail rest)
               in case head (tail stm2) of
                    -- If the first token of the second statement is "(", we parse both statements
                    "(" -> IfStm (makeBExp (tail bexp)) (parseStm stm1) (parseStm (tail stm2))
                    -- Otherwise, we build the second statement from its tokens
                    _ -> IfStm (makeBExp (tail bexp)) (parseStm stm1) [buildStm (tail stm2)]
    -- If the first token is "while", we're building a WhileStm
    "while" ->
      -- We split the list into a boolean expression and the rest of the tokens
      let (bexp, stm) = break (== "do") list
       in case head (tail stm) of
            -- If the first token of the statement is "(", we parse the statement
            "(" -> WhileStm (makeBExp (tail bexp)) (parseStm (tail stm))
            -- Otherwise, we build the statement from its tokens
            _ -> WhileStm (makeBExp (tail bexp)) [buildStm (tail stm)]
    -- If the first token is neither "if" nor "while", we're building an AssStm
    _ ->
      -- We split the list into a variable and an arithmetic expression
      let (var, aexp) = break (== ":=") list
       in AssStm (head var) (makeAExp (tail aexp)) -- We build the AssStm from the variable and the arithmetic expression

-- | The 'findTarget' function takes a list of target tokens and a list of tokens,
-- and returns the position of the first target token that is not nested in parentheses.
findTarget :: [String] -> [String] -> Maybe Int
findTarget targets = locate 0 0
  where
    -- If the list of tokens is empty, we return Nothing because we didn't find a target token.
    locate _ _ [] = Nothing
    locate depth position (token : remainingTokens) =
      case token of
        -- If the token is an opening parenthesis, we increase the depth by 1.
        "(" -> locate (depth + 1) (position + 1) remainingTokens
        -- If the token is "then", we increase the depth by 1.
        "then" -> locate (depth + 1) (position + 1) remainingTokens
        -- If the token is a closing parenthesis, we decrease the depth by 1.
        ")" -> locate (depth - 1) (position + 1) remainingTokens
        -- If the token is "else" and we're not at the top level (depth is not 0),
        -- we decrease the depth by 1.
        "else" | depth /= 0 -> locate (depth - 1) (position + 1) remainingTokens
        _ ->
          -- If we're at the top level (depth is 0) and the token is one of the target tokens,
          -- we return the current position.
          if depth == 0 && (token `elem` targets)
            then Just position
            else -- Otherwise, we continue with the next token, increasing the position by 1.
              locate depth (position + 1) remainingTokens

-- | The 'makeAExp' function takes a list of tokens and builds an arithmetic expression from them.
makeAExp :: [String] -> Aexp
-- If the list contains a single token, it is either a number or a variable.
makeAExp [x] = if all isDigit x then Num (read x) else Var x
makeAExp tokens =
  -- We first try to find a "+" or "-" token, starting from the end of the list.
  case findTarget ["+", "-"] (reverse tokens) of
    Just reversedIndex ->
      -- If we found a "+" or "-" token, we split the list at its position.
      let index = length tokens - reversedIndex - 1
          (before, after) = splitAt index tokens
       in if tokens !! index == "+"
            then -- If the token is "+", we build an AddAexp from the tokens before and after it.
              AddAexp (makeAExp before) (makeAExp (tail after))
            else -- If the token is "-", we build a SubAexp from the tokens before and after it.
              SubAexp (makeAExp before) (makeAExp (tail after))
    Nothing ->
      -- If we didn't find a "+" or "-" token, we try to find a "*" token, starting from the end of the list.
      case findTarget ["*"] (reverse tokens) of
        Just reversedIndex ->
          -- If we found a "*" token, we split the list at its position.
          let index = length tokens - reversedIndex - 1
              (before, after) = splitAt index tokens
           in -- We build a MultAexp from the tokens before and after the "*" token.
              MultAexp (makeAExp before) (makeAExp (tail after))
        -- If we didn't find a "*" token, we remove the first and last token and try again.
        -- This is because the expression must be surrounded by parentheses, which we need to remove.
        Nothing -> makeAExp (tail (init tokens))

-- | The 'makeBExp' function takes a list of tokens and builds a boolean expression from them.
makeBExp :: [String] -> Bexp
-- If the list contains a single token, it is either "True", "False", or an error.
makeBExp [x] =
  case x of
    "True" -> TrueBexp -- If the token is "True", we return a TrueBexp.
    "False" -> FalseBexp -- If the token is "False", we return a FalseBexp.
    _ -> error "Run-time error" -- If the token is anything else, we throw an error.
makeBExp tokens =
  -- We first try to find an "and" token, starting from the end of the list.
  case findTarget ["and"] (reverse tokens) of
    Just reversedIndex ->
      -- If we found an "and" token, we split the list at its position.
      let index = length tokens - reversedIndex - 1
          (before, after) = splitAt index tokens
       in -- We build an AndBexp from the tokens before and after the "and" token.
          AndBexp (makeBExp before) (makeBExp (tail after))
    Nothing ->
      -- If we didn't find an "and" token, we try to find an "=" token, starting from the end of the list.
      case findTarget ["="] (reverse tokens) of
        Just reversedIndex ->
          -- If we found an "=" token, we split the list at its position.
          let index = length tokens - reversedIndex - 1
              (before, after) = splitAt index tokens
           in -- We build an EqBexp from the tokens before and after the "=" token.
              EqBexp (makeBExp before) (makeBExp (tail after))
        Nothing ->
          -- If we didn't find an "=" token, we try to find a "not" token, starting from the end of the list.
          case findTarget ["not"] (reverse tokens) of
            Just reversedIndex ->
              -- If we found a "not" token, we split the list at its position.
              let index = length tokens - reversedIndex - 1
                  after = drop index tokens
               in -- We build a NotBexp from the tokens after the "not" token.
                  NotBexp (makeBExp (tail after))
            Nothing ->
              -- If we didn't find a "not" token, we try to find a "==" token, starting from the end of the list.
              case findTarget ["=="] (reverse tokens) of
                Just reversedIndex ->
                  -- If we found a "==" token, we split the list at its position.
                  let index = length tokens - reversedIndex - 1
                      (before, after) = splitAt index tokens
                   in -- We build an EquBexp from the tokens before and after the "==" token.
                      EquBexp (makeAExp before) (makeAExp (tail after))
                Nothing ->
                  -- If we didn't find a "==" token, we try to find a "<=" token, starting from the end of the list.
                  case findTarget ["<="] (reverse tokens) of
                    Just reversedIndex ->
                      -- If we found a "<=" token, we split the list at its position.
                      let index = length tokens - reversedIndex - 1
                          (before, after) = splitAt index tokens
                       in -- We build a LeBexp from the tokens before and after the "<=" token.
                          LeBexp (makeAExp before) (makeAExp (tail after))
                    -- If we didn't find a "<=" token, we remove the first and last token and try again.
                    -- This is because the expression must be surrounded by parentheses, which we need to remove.
                    Nothing -> makeBExp (tail (init tokens))

parse :: String -> Program
parse input = parseStm (lexer input)