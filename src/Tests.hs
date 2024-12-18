module Tests where

import Assembler
import Compiler
import Data
import Stack
import State


-- To help you test our assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where
    (_, stack, state) = run (code, createEmptyStack, createEmptyState)

-- To help you test our compiler
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str store)
  where
    (_, stack, store) = run (compile (parse programCode), createEmptyStack, createEmptyState)

runAssemblerTests :: IO ()
runAssemblerTests = do
  putStrLn "Test assembler:"
  let tests =
        [ testAssembler [Push 10, Push 4, Push 3, Sub, Mult] == ("-10", ""),
          testAssembler [Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"] == ("", "a=3,someVar=False,var=True"),
          testAssembler [Fals, Store "var", Fetch "var"] == ("False", "var=False"),
          testAssembler [Push (-20), Tru, Fals] == ("False,True,-20", ""),
          testAssembler [Push (-20), Tru, Tru, Neg] == ("False,True,-20", ""),
          testAssembler [Push (-20), Tru, Tru, Neg, Equ] == ("False,-20", ""),
          testAssembler [Push (-20), Push (-21), Le] == ("True", ""),
          testAssembler [Push 5, Store "x", Push 1, Fetch "x", Sub, Store "x"] == ("", "x=4"),
          testAssembler [Push 10, Store "i", Push 1, Store "fact", Loop [Push 1, Fetch "i", Equ, Neg] [Fetch "i", Fetch "fact", Mult, Store "fact", Push 1, Fetch "i", Sub, Store "i"]] == ("", "fact=3628800,i=1")
        ]
  mapM_ (putStrLn . show) tests

-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

runParserTests :: IO ()
runParserTests = do
  putStrLn "Test parser:"
  let tests =
        [ testParser "x := 5; x := x - 1;" == ("", "x=4"),
          testParser "x := 0 - 2;" == ("", "x=-2"),
          testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("", "y=2"),
          testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("", "x=1"),
          testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("", "x=2"),
          testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("", "x=2,z=4"),
          testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("", "x=34,y=68"),
          testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("", "x=34"),
          testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("", "x=1"),
          testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("", "x=2"),
          testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("", "x=2,y=-10,z=6"),
          testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("", "fact=3628800,i=1"),
          testParser "x:=3-1;" == ("", "x=2"),
          testParser "x:=3-1-1; y:=x+1;" == ("", "x=1,y=2"),
          testParser "x:=3-1*3+(2-2*1+1-1);" == ("", "x=0"),
          testParser "x_x:=3-1*3+(2-2*1+1-1);" == ("", "x_x=0"),
          testParser "if (True) then () else ();" == ("", ""),
          testParser "x := 1; (y := 2; z := y+x;);" == ("", "x=1,y=2,z=3"),
          testParser "a := 1071; b := 462; while (not(a == 0)) do (temp := a; while (a <= b) do (b := b - a;); a := b; b := temp;); gcd := b;" == ("", "a=0,b=21,gcd=21,temp=21"),
          testParser "x := 5; if x <= 5 then y := 1; else if x <= 10 then y := 2; else if x <= 15 then y := 3; else y := 4;" == ("", "x=5,y=1"),
          testParser "x := 10; if x <= 5 then y := 1; else if x <= 10 then y := 2; else if x <= 15 then y := 3; else y := 4;" == ("", "x=10,y=2"),
          testParser "x := 15; if x <= 5 then y := 1; else if x <= 10 then y := 2; else if x <= 15 then y := 3; else y := 4;" == ("", "x=15,y=3"),
          testParser "x := 20; if x <= 5 then y := 1; else if x <= 10 then y := 2; else if x <= 15 then y := 3; else y := 4;" == ("", "x=20,y=4")
        ]
  mapM_ (putStrLn . show) tests

runAllTests :: IO ()
runAllTests = do
  runAssemblerTests
  runParserTests