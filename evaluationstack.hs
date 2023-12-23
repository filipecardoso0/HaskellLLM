import Stack 

-- TODO FALTA VERIFICAR SE É UM NUMERO E SE FOR AÍ SIM 
-- FAZER AS OPERAÇÕES 

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