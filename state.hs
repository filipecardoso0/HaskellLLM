module State where 

data StateVariable = StateVariable Char
    deriving (Show, Eq)
data StateVariableVal = StateVariableVal Int 
    deriving (Show, Eq)
type StateTuple = (StateVariable, StateVariableVal) 
type State = [StateTuple]

-- Gets the head of the state list  
getStateHead :: State -> StateTuple
getStateHead (xs) = head(xs)

-- Gets the value bound to var 
getVariableVal :: StateVariable -> State -> StateVariableVal
getVariableVal _ [] = error "Run-time error"
getVariableVal var (x:xs) 
                        | (fst (x) == var) = snd x
                        | otherwise = getVariableVal var (xs)

-- Updates the value bound to var 
updateVariable :: StateVariable -> StateVariableVal -> State -> State
updateVariable _ _ [] = error "Run-time error"
updateVariable var newvarval (x:xs)
                                | (fst x == var) = (var, newvarval) : xs
                                | otherwise = x : updateVariable var newvarval xs 

-- Converts the variable value into an int
variableValToInt :: StateVariableVal -> Int 
variableValToInt (StateVariableVal val) = val 

intToVariableVal :: Int -> StateVariableVal 
intToVariableVal val = StateVariableVal val 

--Replace function for ints 
replace :: Int -> [Int] -> [Int]
replace val [] = []
replace val (x:xs) 
                | val == x = 3 : xs
                | otherwise = x : replace val xs

-- Generates an Empty State
createEmptyState :: State 
createEmptyState = []

