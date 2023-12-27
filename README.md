# Haskell Low Level Machine
## Group Project: T06_G10


Filipe de Azevedo Cardoso (<ins>**up202006409**</ins>)

- Contribution: 50%

José Pedro ALmeida Batista de Sousa Santos (<ins>**up202108673**</ins>)

- Contribution: 50%.

### Part 1 
#### Data Decisions

On part 1 two data structures were created. Stack and State. <br>  -Stack needed to Handle both Booleans and Integers, once it could have the values "tt" and 1, 2, ... . Therefore, a new data called StackElement was created, being defined as StackInt Int and StackString String. StackInt would handle all the Int data and StackInt all the Boolean or "tt"/"ff" (in it's final state). Finally, the type Stack was defined as a list of StackElements. Hence, a new data type was not needed, as Stack is just a ALIAS for [StackElement]. <br> - State followed the same strategy. It needed to handle both Integers, Booleans and Strings. Thus, a data type called StateVariable (to handle variables) was created. In addition StateVariableVal, having the types StateVariableValInt and StateVariableValBool were created, as well, in order to handle the variables values (Int or Bool). To an extent, the State was a list of tuples of (StateVariable, StateVariableVal), so the type StateTuple was defined as mentioned and the type State was simply a list of StateTuple ([StateTuple]).

#### Functions Defined: 
Due to the data types created functions to go to the vanilla type or accessing the desired variable value had to be created. As an example the functions are: 

- State.hs

```hs
-- Converts Bool to StateVariableValBool
boolToVariableVal :: Bool -> StateVariableVal

-- Converts StateVariableValInt to Int 
variableValToInt :: StateVariableVal -> Int 

-- Converts Int to StateVariableValInt 
intToVariableVal :: Int -> StateVariableVal

-- Converts Bool to StateVariableValBool
boolToVariableVal :: Bool -> StateVariableVal

-- Converts the StateVariable into a String
variableToString :: StateVariable -> String

-- Converts StateVariableVal into a String
stateVariableVal2Str :: StateVariableVal -> String
```

- Stack.hs

```hs
-- Converts a StackElementInt back to Int 
fromStackElementInt :: StackElement -> Int 

-- Converts a StackElementString back to String
fromStackElementString :: StackElement -> String

-- Assigns a StackElementString to its correct Boolean Value
stackElementStringToBool :: String -> Bool 

-- Once our data type uses strings we translate the "tt" and "ff" values to the expected output ones
outputCorrectValue :: String -> String
```

A very important function had to be created in the case of the State Data Structure due to the fetch and store functions, who were hable to handle StateVariables that were both Int or Bool (fetch function).

```hs
-- Detects if wheter the VariableValType is Bool or not
isBoolVariableValType :: StateVariableVal -> Bool 
isBoolVariableValType (StateVariableValBool _) = True 
isBoolVariableValType _ = False 

-- pushes the value bound to var onto the stack
fetch :: StateVariable -> State -> Stack -> Stack
fetch var state stack           
                | isBoolVariableValType variableval = pushBool (variableValToBool variableval) stack
                | otherwise = pushInt (variableValToInt variableval) stack
                where variableval = getVariableVal var state
```

The same was done in the Stack data structure with the aid of the isNumber function in order to ease the store process. This hadn't to be done in the push function, once the assembler only pushed integer values through the push-x instruction and booleans through Tru and Fals instructions. So, it could be easily done just by creating a pushInt and pushBool function

```hs
-- Detects if wheter the VariableValType is Bool or not
-- Verify if the element on the stack is a number
isNumber :: StackElement -> Bool 
isNumber (StackInt num) = True 
isNumber _ = False

-- pops the topmost element of the stack and updates the State so that the popped value is bound to var
store :: StateVariable -> Stack -> State -> (Stack, State)
store var stack state = (newstack, newstate)
        where newstack = pop(stack) 
              newstate 
                      | isNumber topelem = updateVariable var (intToVariableVal(fromStackElementInt(top stack))) state
                      | otherwise = updateVariable var (boolToVariableVal(stackElementStringToBool(fromStackElementString(top stack)))) state
                      where topelem = top stack

-- Pushes an Int/Integer into the Stack 
pushInt :: Integral a => a -> Stack -> Stack
pushInt num xs = StackInt (fromIntegral num) : xs

-- Pushes a String into the Stack
pushBool :: Bool -> Stack -> Stack
pushBool True xs = StackString "tt" : xs
pushBool False xs = StackString "ff" : xs
```


<!--detailing the
decisions made when defining the data and functions defined in the program-->
