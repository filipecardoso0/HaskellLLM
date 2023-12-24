module Stack (Stack , pushInt, pushBool, pop, top, empty, isEmpty, size, fromStackElementInt, fromStackElementString, isNumber) where 


data StackElement = StackInt Int | StackString String
    deriving (Show, Eq, Ord)

type Stack = [StackElement]

-- Pushes an Int/Integer into the Stack 
pushInt :: Integral a => a -> Stack -> Stack
pushInt num xs = StackInt (fromIntegral num) : xs

-- Pushes a String into the Stack
pushBool :: Bool -> Stack -> Stack
pushBool True xs = StackString "tt" : xs
pushBool False xs = StackString "ff" : xs

-- Gets the top element of the Stack 
top :: Stack -> StackElement
top (x:_) = x
top _ = error "Stack.top: Stack is Empty"

-- Verifies if the Stack is empty 
isEmpty :: Stack -> Bool 
isEmpty (x:_) = False 
isEmpty _ = True 

-- Generates a empty Stack 
createEmptyStack :: Stack 
createEmptyStack = []

-- Pops an element out of the Stack 
pop :: Stack -> Stack
pop (_:xs) = xs
pop _ = error "Stack.pop: Stack is empty"

-- Gets the ammount of elements inside the Stack 
size :: Stack -> Int
size (xs) = length xs

-- Converts a StackElementString back to String
fromStackElementInt :: StackElement -> Int 
fromStackElementInt (StackInt num) = num
fromStackElementInt _ = error("Stack.fromStackElementInt: Not a number")

-- Converts a StackElementInt back to Int 
fromStackElementString :: StackElement -> String
fromStackElementString (StackString str) = str
fromStackElementString _ = error("Stack.fromStackElementString: Not a string")

isNumber :: StackElement -> Bool 
isNumber (StackInt num) = True 
isNumber _ = False