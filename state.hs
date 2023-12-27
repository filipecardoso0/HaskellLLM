module State where 

import Data.List (sortOn)

data StateVariable = StateVariable String
    deriving (Show, Eq)
data StateVariableVal = StateVariableValInt Int | StateVariableValBool Bool 
    deriving (Show, Eq)
type StateTuple = (StateVariable, StateVariableVal) 
type State = [StateTuple]

-- Generates an Empty State
createEmptyState :: State 
createEmptyState = []

-- Gets the head of the state list  
getStateHead :: State -> StateTuple
getStateHead (xs) = head(xs)

-- Gets the value bound to var 
getVariableVal :: StateVariable -> State -> StateVariableVal
getVariableVal _ [] = error "Run-time error"
getVariableVal var (x:xs) 
                        | (fst (x) == var) = snd x
                        | otherwise = getVariableVal var (xs)

-- Detects if wheter the VariableValType is Bool or not
isBoolVariableValType :: StateVariableVal -> Bool 
isBoolVariableValType (StateVariableValBool _) = True 
isBoolVariableValType _ = False 

-- Converts StateVariableValInt to Int 
variableValToInt :: StateVariableVal -> Int 
variableValToInt (StateVariableValInt val) = val 
variableValToInt _ = error "Run-time error"

-- Converts StateVariableValBool to Bool
variableValToBool :: StateVariableVal -> Bool 
variableValToBool (StateVariableValBool val) = val 
variableValToBool _ = error "Run-time error"

-- Converts Int to StateVariableValInt 
intToVariableVal :: Int -> StateVariableVal
intToVariableVal val = (StateVariableValInt val)

-- Converts Bool to StateVariableValBool
boolToVariableVal :: Bool -> StateVariableVal
boolToVariableVal val = (StateVariableValBool val)

-- Updates the value bound to var 
updateVariable :: StateVariable -> StateVariableVal -> State -> State
updateVariable _ _ [] = error "Run-time error"
updateVariable var newvarval (x:xs)
                                | (fst x == var) = (var, newvarval) : xs
                                | otherwise = x : updateVariable var newvarval xs 

-- Converts the StateVariable into a String
variableToString :: StateVariable -> String
variableToString (StateVariable var) = var

-- Converts the StateValuesVar into String
convertStateValuesVar :: State -> [(String, StateVariableVal)]
convertStateValuesVar [] = []
convertStateValuesVar (x:xs) = (variableToString(fst x), snd x) : convertStateValuesVar xs 

-- Sorts the Tuple List according to the Variable type
sortState :: State -> [(String, StateVariableVal)]
sortState state = sortOn fst (convertStateValuesVar state)

-- Converts StateVariableVal into a String
stateVariableVal2Str :: StateVariableVal -> String
stateVariableVal2Str val
                    | isBoolVariableValType val = show(variableValToBool val)
                    | otherwise = show(variableValToInt val)

-- Turns the Converted State Data into a String 
state2StrAux :: [(String, StateVariableVal)] -> String
state2StrAux [] = []
state2StrAux (x:[]) = ((fst x) ++ "=" ++ stateVariableVal2Str (snd x)) ++ state2StrAux [] 
state2StrAux (x:xs) = ((fst x) ++ "=" ++ stateVariableVal2Str (snd x)) ++ "," ++ state2StrAux xs 

-- Turns the Raw State into a String using the auxiliary function state2StrAux
state2Str :: State -> String
state2Str state = state2StrAux (sortState state)
