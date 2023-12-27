import Stack 
import State 

type EvaluationStack = (Stack, State)
type BranchOutput = (Stack, Code)

{- ORGANIZAR MELHOR O CODIGO -> PRECISA DE SER MUITO MELHOR ORGANIZADO -}
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

{- A LOT MORE OF CODE COMENTING NEEDS TO BE DONE -}

add :: Stack -> Stack 
add stack = if (isNumber(elem1) && isNumber(elem2)) 
            then (pushInt((fromStackElementInt elem1)+(fromStackElementInt elem2)) updatedStack)
            else (error "Run-time error")
    where elem1 = top(stack)
          elem2 = top(pop(stack))
          updatedStack = pop(pop(stack))

sub :: Stack -> Stack 
sub stack = if (isNumber(elem1) && isNumber(elem2)) 
            then (pushInt((fromStackElementInt elem1)-(fromStackElementInt elem2)) updatedStack)
            else (error "Run-time error")
    where elem1 = top(stack)
          elem2 = top(pop(stack))
          updatedStack = pop(pop(stack))

mult :: Stack -> Stack 
mult stack = if (isNumber(elem1) && isNumber(elem2)) 
            then (pushInt((fromStackElementInt elem1)*(fromStackElementInt elem2)) updatedStack)
            else (error "Run-time error")
    where elem1 = top(stack)
          elem2 = top(pop(stack))
          updatedStack = pop(pop(stack))

eq :: Stack -> Stack 
eq stack = pushBool((elem1 == elem2)) updatedStack
    where 
        elem1 = top(stack)
        elem2 = top(pop(stack))
        updatedStack = pop(pop(stack))

le :: Stack -> Stack 
le stack = if (isNumber(elem1) && isNumber(elem2)) 
           then (pushBool((elem1 <= elem2)) updatedStack) 
           else (error "Run-time error")
    where 
        elem1 = top(stack)
        elem2 = top(pop(stack))
        updatedStack = pop(pop(stack))

-- pushes the value bound to var onto the stack
fetch :: StateVariable -> State -> Stack -> Stack
fetch var state stack           
                | isBoolVariableValType variableval = pushBool (variableValToBool variableval) stack
                | otherwise = pushInt (variableValToInt variableval) stack
                where variableval = getVariableVal var state

-- pops the topmost element of the stack and updates the State so that the popped value is bound to var
store :: StateVariable -> Stack -> State -> EvaluationStack
store var stack state = (newstack, newstate)
        where newstack = pop(stack) 
              newstate 
                      | isNumber topelem = updateVariable var (intToVariableVal(fromStackElementInt(top stack))) state
                      | otherwise = updateVariable var (boolToVariableVal(stackElementStringToBool(fromStackElementString(top stack)))) state
                      where topelem = top stack

-- returns the input stack and state (state)
noop :: Stack -> State -> EvaluationStack
noop stack state = (stack, state)

-- Conditional Statement (A boolean value (kind of) has to be specified)
branch :: Code -> Code -> Stack -> BranchOutput 
branch c1 c2 stack
        | fromStackElementString(top(stack)) == "tt" = (pop(stack), c1)
        | fromStackElementString(top(stack)) == "ff" = (pop(stack), c2)
        | otherwise = error "Run-time error"

-- Loop 
loop :: Code -> Code -> Code 
loop c1 c2 = c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]