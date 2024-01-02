module Stack (Stack, pushInt, pushBool, pop, top, createEmptyStack, isEmpty, size, fromStackElementInt, fromStackElementString, stackElementStringToBool, isNumber, stack2Str) where

data StackElement = StackInt Int | StackString String
  deriving (Show, Eq, Ord)

type Stack = [StackElement]

-- Pushes an Int/Integer into the Stack
pushInt :: (Integral a) => a -> Stack -> Stack
pushInt num xs = StackInt (fromIntegral num) : xs

-- Pushes a String into the Stack
pushBool :: Bool -> Stack -> Stack
pushBool True xs = StackString "tt" : xs
pushBool False xs = StackString "ff" : xs

-- Gets the top element of the Stack
top :: Stack -> StackElement
top (x : _) = x
top _ = error "Run-time error"

-- Verifies if the Stack is empty
isEmpty :: Stack -> Bool
isEmpty (x : _) = False
isEmpty _ = True

-- Generates a empty Stack
createEmptyStack :: Stack
createEmptyStack = []

-- Pops an element out of the Stack
pop :: Stack -> Stack
pop (_ : xs) = xs
pop _ = error "Run-time error"

-- Gets the ammount of elements inside the Stack
size :: Stack -> Int
size (xs) = length xs

-- Converts a StackElementInt back to Int
fromStackElementInt :: StackElement -> Int
fromStackElementInt (StackInt num) = num
fromStackElementInt _ = error "Run-time error"

-- Converts a StackElementString back to String
fromStackElementString :: StackElement -> String
fromStackElementString (StackString str) = str
fromStackElementString _ = error "Run-time error"

-- Assigns a StackElementString to its correct Boolean Value
stackElementStringToBool :: String -> Bool
stackElementStringToBool str
  | str == "tt" = True
  | str == "ff" = False

-- Verify if the element on the stack is a number
isNumber :: StackElement -> Bool
isNumber (StackInt num) = True
isNumber _ = False

-- Once our data type uses strings we translate the "tt" and "ff" values to the expected output ones
outputCorrectValue :: String -> String
outputCorrectValue "tt" = "True"
outputCorrectValue "ff" = "False"
outputCorrectValue _ = error "Run-time error"

-- Outputs the Stacks contents in a string
stack2Str :: Stack -> String
stack2Str [] = []
stack2Str (x : [])
  | isNumber (x) == True = (show (fromStackElementInt x)) ++ [] -- Is a Numeric Value
  | otherwise = outputCorrectValue (fromStackElementString (x)) ++ [] -- Is a Boolean String Element "tt" or "ff"
stack2Str (x : xs)
  | isNumber (x) == True = (show (fromStackElementInt x)) ++ "," ++ (stack2Str xs) -- Is a Numeric Value
  | otherwise = outputCorrectValue (fromStackElementString (x)) ++ "," ++ (stack2Str xs) -- Is a Boolean String Element "tt" or "ff"