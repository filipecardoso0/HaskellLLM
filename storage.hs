module Storage where 

data StorageVariable = StorageVariable Char
    deriving (Show, Eq)
data StorageVariableVal = StorageVariableVal Int 
    deriving (Show, Eq)
type StorageTuple = (StorageVariable, StorageVariableVal) 
type Storage = [StorageTuple]

-- Gets the head of the storage list  
getStorageHead :: Storage -> StorageTuple
getStorageHead (xs) = head(xs)

-- Gets the value bound to var 
getVariableVal :: StorageVariable -> Storage -> StorageVariableVal
getVariableVal _ [] = error "Run-time error"
getVariableVal var (x:xs) 
                        | (fst (x) == var) = snd x
                        | otherwise = getVariableVal var (xs)

-- Updates the value bound to var 
updateVariable :: StorageVariable -> StorageVariableVal -> Storage -> Storage
updateVariable _ _ [] = error "Run-time error"
updateVariable var newvarval (x:xs)
                                | (fst x == var) = (var, newvarval) : xs
                                | otherwise = x : updateVariable var newvarval xs 

-- Converts the variable value into an int
variableValToInt :: StorageVariableVal -> Int 
variableValToInt (StorageVariableVal val) = val 

intToVariableVal :: Int -> StorageVariableVal 
intToVariableVal val = StorageVariableVal val 

--Replace function for ints 
replace :: Int -> [Int] -> [Int]
replace val [] = []
replace val (x:xs) 
                | val == x = 3 : xs
                | otherwise = x : replace val xs

-- Generates an Empty Storage
createEmptyState :: Storage 
createEmptyState = []

