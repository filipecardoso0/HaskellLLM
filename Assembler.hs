module Assembler where

import Program
import State
    ( boolToVariableVal,
      getVariableVal,
      intToVariableVal,
      isBoolVariableValType,
      str2StateVar,
      updateVariable,
      variableValToBool,
      variableValToInt,
      State,
      StateVariable )import State

data Inst
  = Push Integer
  | Add
  | Mult
  | Sub
  | Tru
  | Fals
  | Equ
  | Le
  | And
  | Neg
  | Fetch String
  | Store String
  | Noop
  | Branch Code Code
  | Loop Code Code
  deriving (Show)

type Code = [Inst]

-- Performs the Addition between the top two elements of the stack
add :: Stack -> Stack
add stack =
  if (isNumber (elem1) && isNumber (elem2))
    then (pushInt ((fromStackElementInt elem1) + (fromStackElementInt elem2)) updatedStack)
    else (error "Run-time error")
  where
    elem1 = top (stack)
    elem2 = top (pop (stack))
    updatedStack = pop (pop (stack))

-- Performs the Subtraction between the top two values of the stack (topone-2ndtopone)
sub :: Stack -> Stack
sub stack =
  if (isNumber (elem1) && isNumber (elem2))
    then (pushInt ((fromStackElementInt elem1) - (fromStackElementInt elem2)) updatedStack)
    else (error "Run-time error")
  where
    elem1 = top (stack)
    elem2 = top (pop (stack))
    updatedStack = pop (pop (stack))

-- Performs the multiplication operation between the two top elements of the stack
mult :: Stack -> Stack
mult stack =
  if (isNumber (elem1) && isNumber (elem2))
    then (pushInt ((fromStackElementInt elem1) * (fromStackElementInt elem2)) updatedStack)
    else (error "Run-time error")
  where
    elem1 = top (stack)
    elem2 = top (pop (stack))
    updatedStack = pop (pop (stack))

-- Performs the equalization operation between the top two values of the stack
eq :: Stack -> Stack
eq stack = pushBool ((elem1 == elem2)) updatedStack
  where
    elem1 = top (stack)
    elem2 = top (pop (stack))
    updatedStack = pop (pop (stack))

-- Performs the less or equal than between the top value and the second top element of the stack
le :: Stack -> Stack
le stack =
  if (isNumber (elem1) && isNumber (elem2))
    then (pushBool ((elem1 <= elem2)) updatedStack)
    else (error "Run-time error")
  where
    elem1 = top (stack)
    elem2 = top (pop (stack))
    updatedStack = pop (pop (stack))

-- Negates the topmost element of the stack
neg :: Stack -> Stack
neg stack
  | fromStackElementString (top (stack)) == "tt" = pushBool False (pop (stack))
  | fromStackElementString (top (stack)) == "ff" = pushBool True (pop (stack))

-- pushes the value bound to var onto the stack
fetch :: StateVariable -> State -> Stack -> Stack
fetch var state stack
  | isBoolVariableValType variableval = pushBool (variableValToBool variableval) stack
  | otherwise = pushInt (variableValToInt variableval) stack
  where
    variableval = getVariableVal var state

-- pops the topmost element of the stack and updates the State so that the popped value is bound to var
store :: StateVariable -> Stack -> State -> (Stack, State)
store var stack state = (newstack, newstate)
  where
    newstack = pop (stack)
    newstate
      | isNumber topelem = updateVariable var (intToVariableVal (fromStackElementInt (top stack))) state
      | otherwise = updateVariable var (boolToVariableVal (stackElementStringToBool (fromStackElementString (top stack)))) state
      where
        topelem = top stack

-- returns the input stack and state (state)
noop :: Stack -> State -> (Stack, State)
noop stack state = (stack, state)

-- Conditional Statement (A boolean value (kind of) has to be specified)
branch :: Code -> Code -> Stack -> (Stack, Code)
branch c1 c2 stack
  | fromStackElementString (top (stack)) == "tt" = (pop (stack), c1)
  | fromStackElementString (top (stack)) == "ff" = (pop (stack), c2)
  | otherwise = error "Run-time error"

-- Loop
loop :: Code -> Code -> Code
loop c1 c2 = c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]

-- Pushes True in to the top of the stack
tru :: Stack -> Stack
tru stack = pushBool True stack

-- Pushes False in to the top of the stack
fals :: Stack -> Stack
fals stack = pushBool False stack

-- Performs the and opperation of the elements on top of the stack
ande :: Stack -> Stack
ande stack = pushBool res (pop (pop (stack))) -- Pushes the element after popping the two top elements out of the stack
  where
    res = stackElementStringToBool (fromStackElementString (top (stack))) && stackElementStringToBool (fromStackElementString (top (pop (stack))))

-- run (Code, Stack, State) -> (Code, Stack, State)
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (x : xs, stack, state) =
  case x of
    Add -> run (xs, (add stack), state)
    Sub -> run (xs, (sub stack), state)
    Mult -> run (xs, (mult stack), state)
    Neg -> run (xs, (neg stack), state)
    Tru -> run (xs, (tru stack), state)
    Fals -> run (xs, (fals stack), state)
    Equ -> run (xs, (eq stack), state)
    Le -> run (xs, (le stack), state)
    And -> run (xs, (ande stack), state)
    Push val -> run (xs, (pushInt val stack), state)
    Fetch val -> run (xs, (fetch (str2StateVar val) state stack), state)
    Store val -> do
      let (newstack, newstate) = store (str2StateVar val) stack state
      run (xs, newstack, newstate)
    Noop -> do
      let (newstack, newstate) = noop stack state
      run (xs, newstack, newstate)
    Branch c1 c2 -> do
      let (newstack, newxs) = branch c1 c2 stack
      run ((newxs ++ xs), newstack, state)
    Loop c1 c2 -> run (loop c1 c2 ++ xs, stack, state)