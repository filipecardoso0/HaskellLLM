import Stack 
import Storage 

type EvaluationStack = (Stack, Storage)

{- A LOT MORE OF CODE COMENTING NEEDS TO BE DONE -}
{- TODO: RENAME STORAGE TO STATE -}

add :: Stack -> Stack 
add stack = if (isNumber(elem1) && isNumber(elem2)) 
            then (pushInt((fromStackElementInt elem1)+(fromStackElementInt elem2)) updatedStack)
            else (error "EvaluationStack.add: Add operation can only be performed if the topmost elements of the stack are numbers")
    where elem1 = top(stack)
          elem2 = top(pop(stack))
          updatedStack = pop(pop(stack))

sub :: Stack -> Stack 
sub stack = if (isNumber(elem1) && isNumber(elem2)) 
            then (pushInt((fromStackElementInt elem1)-(fromStackElementInt elem2)) updatedStack)
            else (error "EvaluationStack.add: Subctraction operation can only be performed if the topmost elements of the stack are numbers")
    where elem1 = top(stack)
          elem2 = top(pop(stack))
          updatedStack = pop(pop(stack))

mult :: Stack -> Stack 
mult stack = if (isNumber(elem1) && isNumber(elem2)) 
            then (pushInt((fromStackElementInt elem1)*(fromStackElementInt elem2)) updatedStack)
            else (error "EvaluationStack.mult: Multiplication operation can only be performed if the topmost elements of the stack are numbers")
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
           else (error "EvaluationStack.le: Less than or equal (<=) can only be performed only if the topmost elements of the stack are numbers")
    where 
        elem1 = top(stack)
        elem2 = top(pop(stack))
        updatedStack = pop(pop(stack))

-- Pushes the value bound to var onto the stack 
fetch :: StorageVariable -> Storage -> Stack -> Stack
fetch var strg stack = pushInt val stack 
        where val = variableValToInt(getVariableVal var strg)

-- pops the topmost element of the stack and updates the storage so that the popped value is bound to x
store :: StorageVariable -> Stack -> Storage -> EvaluationStack
store var stack storage = (newstack, newstorage)
        where newstack = pop(stack) 
              newstorage = updateVariable var (intToVariableVal(fromStackElementInt(top stack))) storage

-- returns the input stack and state (storage)
noop :: Stack -> Storage -> EvaluationStack
noop stack storage = (stack, storage)
