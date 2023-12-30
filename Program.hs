module Program where

import Assembler
import Data.Char (isDigit)
import Data.List (elemIndex, isPrefixOf)
import Text.Parsec.Token qualified as Tok


data Aexp = 
    Num Integer |
    Var String | 
    AddAexp Aexp Aexp | 
    MultAexp Aexp Aexp | 
    SubAexp Aexp Aexp 
    deriving (Show)

data Bexp = 
    TrueBexp | 
    FalseBexp | 
    NotBexp Bexp | 
    EquBexp Aexp Aexp | 
    EqBexp Bexp Bexp |
    LeBexp Aexp Aexp | 
    AndBexp Bexp Bexp 
    deriving (Show)

data Stm = 
    AssStm String Aexp | 
    IfStm Bexp [Stm] [Stm] | 
    WhileStm Bexp [Stm] 
    deriving (Show)

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
    | otherwise = 
    case head str of
        ' ' -> lexer (tail str)
        '(' -> "(" : lexer (tail str)
        ')' -> ")" : lexer (tail str)
        ';' -> ";" : lexer (tail str)
        '=' -> "=" : lexer (tail str)
        '+' -> "+" : lexer (tail str)
        '-' -> "-" : lexer (tail str)
        '*' -> "*" : lexer (tail str)
        _ -> (head str :
            takeWhile (\x -> x /= ' ' && x /= '(' && x /= ')' && x /= ';' && x /= '=' && x /= '+' && x /= '-' && x /= '*' && x /= '<' && x /= ':') (tail str)) :
            lexer (dropWhile (\x -> x /= ' ' && x /= '(' && x /= ')' && x /= ';' && x /= '=' && x /= '+' && x /= '-' && x /= '*' && x /= '<' && x /= ':') (tail str))

buildData :: [String] -> [Stm]
buildData [] = []
buildData list = case findNotInner [";"] list of
  Just index -> 
    let (stm, rest) = splitAt index list 
    in if head stm == "(" 
       then buildData (tail (init stm)) 
       else case rest of
         [_] -> [buildStm stm] 
         _ -> buildStm stm : buildData (tail rest)
  Nothing -> buildData (tail (init list))

buildStm :: [String] -> Stm
buildStm list = 
    case head list of
        "if" -> 
            let (bexp, rest) = break (== "then") list 
            in case findNotInner ["else"] (tail rest) of
                Just index -> 
                    let (stm1, stm2) = splitAt index (tail rest) 
                    in case head (tail stm2) of
                        "(" -> IfStm (buildBexp (tail bexp)) (buildData stm1) (buildData (tail stm2))
                        _ -> IfStm (buildBexp (tail bexp)) (buildData stm1) [buildStm (tail stm2)]
        "while" -> 
            let (bexp, stm) = break (== "do") list 
            in case head (tail stm) of
                "(" -> WhileStm (buildBexp (tail bexp)) (buildData (tail stm))
                _ -> WhileStm (buildBexp (tail bexp)) [buildStm (tail stm)]
        _ -> 
            let (var, aexp) = break (== ":=") list 
            in AssStm (head var) (buildAexp (tail aexp))

findNotInner :: [String] -> [String] -> Maybe Int
findNotInner targets = find 0 0
  where
    find _ _ [] = Nothing
    find depth index (x:rest) =
        case x of
        "(" -> find (depth + 1) (index + 1) rest
        "then" -> find (depth + 1) (index + 1) rest
        ")" -> find (depth - 1) (index + 1) rest
        "else" | depth /= 0 -> find (depth - 1) (index + 1) rest
        _ ->
            if depth == 0 && (x `elem` targets)
                then Just index
                else find depth (index + 1) rest

buildAexp :: [String] -> Aexp
buildAexp [x] = if all isDigit x then Num (read x) else Var x
buildAexp list = 
    case findNotInner ["+","-"] (reverse list) of
        Just reversedIndex -> 
            let index = length list - reversedIndex - 1
                (before, after) = splitAt index list
            in if list!!index == "+"
                then AddAexp (buildAexp before) (buildAexp (tail after))
                else SubAexp (buildAexp before) (buildAexp (tail after))
        Nothing -> 
            case findNotInner ["*"] (reverse list) of
                Just reversedIndex -> 
                    let index = length list - reversedIndex - 1
                        (before, after) = splitAt index list
                    in MultAexp (buildAexp before) (buildAexp (tail after))
                Nothing -> buildAexp (tail (init list))

buildBexp :: [String] -> Bexp
buildBexp [x] = 
    case x of
        "True" -> TrueBexp
        "False" -> FalseBexp
        _ -> error "Run-time error"
buildBexp list = 
    case findNotInner ["and"] (reverse list) of
        Just reversedIndex -> 
            let index = length list - reversedIndex - 1
                (before, after) = splitAt index list
            in AndBexp (buildBexp before) (buildBexp (tail after))
        Nothing -> 
            case findNotInner ["="] (reverse list) of
                Just reversedIndex -> 
                    let index = length list - reversedIndex - 1
                        (before, after) = splitAt index list
                    in EqBexp (buildBexp before) (buildBexp (tail after))
                Nothing -> 
                    case findNotInner ["not"] (reverse list) of
                        Just reversedIndex -> 
                            let index = length list - reversedIndex - 1
                                after = drop index list
                            in NotBexp (buildBexp (tail after))
                        Nothing -> 
                            case findNotInner ["=="] (reverse list) of
                                Just reversedIndex -> 
                                    let index = length list - reversedIndex - 1
                                        (before, after) = splitAt index list
                                    in EquBexp (buildAexp before) (buildAexp (tail after))
                                Nothing -> 
                                    case findNotInner ["<="] (reverse list) of
                                        Just reversedIndex -> 
                                            let index = length list - reversedIndex - 1
                                                (before, after) = splitAt index list
                                            in LeBexp (buildAexp before) (buildAexp (tail after))
                                        Nothing -> buildBexp (tail (init list))

parse :: String -> [Stm]
parse = buildData . lexer