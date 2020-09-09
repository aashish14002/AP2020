-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone

env :: Env
env = (extendEnv "a" 3 (extendEnv "b" 4 initEnv))

tests :: [(String, Bool)]
tests = [testShowExp1, testShowExp2, testShowExp3,
          testevalSimple1, testevalSimple2, testevalSimple3, testevalSimple4, 
          testevalSimple5, testevalSimple6,
          testextendEnv1, testextendEnv2, testextendEnv3, testextendEnv4, 
          testevalFull1, testevalFull2, testevalFull3, testevalFull4, 
          testevalFull5, testevalFull6, testevalFull7, testevalFull8, 
          testevalFull9, testevalFull10, testevalFull11, testevalFull12, 
          testevalFull13, testevalFull14,
          testevalErr1, testevalErr2, testevalErr3, testevalErr4, testevalErr5, 
          testevalErr6, testevalErr7, testevalErr8, testevalErr9, 
          testevalErr10, testevalErr11, testevalErr12, testevalErr13, 
          testevalErr14, testevalErr15, testevalErr16, testevalErr17
        ] where

---------------------------------------
-- tests for showExp
---------------------------------------
  testShowExp1 = ("testShowExp1", showExp (Cst (-4)) == "(-4)")
  testShowExp2 = ("testShowExp2", showExp (Cst (5)) == "5")
  testShowExp3 = ("testShowExp3", showExp (Div (Cst (-2)) (Sub (Cst 3) (Cst 4))) == "((-2)/(3-4))")

---------------------------------------
-- tests for evalSimple
---------------------------------------
  testevalSimple1 = ("testevalSimple1", evalSimple (Cst (-7)) == (-7))
  testevalSimple2 = ("testevalSimple2", evalSimple (Add (Cst 12) (Cst 24)) == 36)
  testevalSimple3 = ("testevalSimple3", 
    evalSimple (Sub (Add (Cst 2) (Cst 3)) (Cst 5)) == 0)
  testevalSimple4 = ("testevalSimple4", 
    evalSimple (Pow (Cst 2) (Add (Cst (-1))(Cst 1))) == 1)
  testevalSimple5 = ("testevalSimple5", evalSimple (Pow (Cst 0) (Cst 0)) == 1)
  testevalSimple6 = ("testevalSimple6", 
    evalSimple (Div (Cst 1) (Mul (Cst 2)(Cst 1))) == 0)

---------------------------------------
-- tests for extendEnv
---------------------------------------
  testextendEnv1 = ("testextendEnv1", (extendEnv "a" 9 initEnv) "a" == Just 9)
  testextendEnv2 = ("testextendEnv2", (extendEnv "a" 4 initEnv) "z" == Nothing)
  testextendEnv3 = ("testextendEnv3", env "a" == Just 3)
  testextendEnv4 = ("testextendEnv4", env "z" == Nothing)

---------------------------------------
-- tests for evalFull
---------------------------------------
  testevalFull1 = ("testevalFull1", 
    evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42)
  testevalFull2 = ("testevalFull2", evalFull (Cst (-7)) env == (-7))
  testevalFull3 = ("testevalFull3", evalFull (Add (Cst 12) (Cst 24)) env == 36)
  testevalFull4 = ("testevalFull4", 
    evalFull (Sub (Add (Cst 2) (Cst 3)) (Cst 5)) env == 0)
  testevalFull5 = ("testevalFull5", 
    evalFull (Pow (Cst 2) (Add (Cst (-1))(Cst 1))) env == 1)
  testevalFull6 = ("testevalFull6", evalFull (Pow (Cst 0) (Cst 0)) env == 1)
  testevalFull7 = ("testevalFull7", 
    evalFull (Div (Cst 12) (Mul (Cst (-2))(Cst 1))) env == (-6))
  testevalFull8 = ("testevalFull8", 
    evalFull (Let "x" (Pow (Cst 3) (Cst (-1))) (Var "b")) env  == 4)
  testevalFull9 = ("testevalFull9", 
    evalFull (Let "x" (Div (Cst 3) (Cst 3)) (Add (Var "x")(Var "b"))) env  == 5)
  testevalFull10 = ("testevalFull10", evalFull (Var "a") env == 3)
  testevalFull11 = ("testevalFull11", 
    evalFull (If (Cst 0) (Div (Cst 1) (Cst 0)) (Cst 2)) env == 2)
  testevalFull12 = ("testevalFull12", 
    evalFull (If (Cst (1)) (Cst 1) (Div (Cst 1) (Cst 0))) env == 1)
  testevalFull13 = ("testevalFull13", 
    evalFull (Sum "a" (Cst 1) (Cst 3) (Cst 5)) env == 15)
  testevalFull14 = ("testevalFull14", 
    evalFull (Sum "x" (Cst 0) (Add (Cst 2) (Cst 2)) (Mul (Var "x") (Var "x"))) env == 30)

---------------------------------------
-- tests for evalErr
---------------------------------------
  testevalErr1 = ("testevalErr1", 
    evalErr (Let "a" (Cst 42) (Var "a")) initEnv == Right 42)
  testevalErr2 = ("testevalErr2", evalErr (Cst (-7)) env == Right (-7))
  testevalErr3 = ("testevalErr3", evalErr (Add (Cst 12) (Cst 24)) env == Right 36)
  testevalErr4 = ("testevalErr4", 
    evalErr (Sub (Add (Cst 2) (Cst 3)) (Cst 5)) env == Right 0)
  testevalErr5 = ("testevalErr5", 
    evalErr (Pow (Cst 2) (Add (Cst (-1))(Cst 1))) env == Right 1)
  testevalErr6 = ("testevalErr6", evalErr (Pow (Cst 0) (Cst 0)) env == Right 1)
  testevalErr7 = ("testevalErr7", 
    evalErr (Div (Cst 12) (Mul (Cst (-2))(Cst 1))) env == Right (-6))
  testevalErr8 = ("testevalErr8", 
    evalErr (Let "x" (Pow (Cst 3) (Cst (-1))) (Var "b")) env  == Left ENegPower)
  testevalErr9 = ("testevalErr9", 
    evalErr (Let "x" (Div (Cst 3) (Cst 3)) (Add (Var "x")(Var "b"))) env  == Right 5)
  testevalErr10 = ("testevalErr10", evalErr (Var "a") env == Right 3)
  testevalErr11 = ("testevalErr11", 
    evalErr (If (Cst 0) (Div (Cst 1) (Cst 0)) (Cst 2)) env == Right 2)
  testevalErr12 = ("testevalErr12", 
    evalErr (If (Cst (1)) (Cst 1) (Div (Cst 1) (Cst 0))) env == Right 1)
  testevalErr13 = ("testevalErr13", 
    evalErr (Sum "a" (Cst 1) (Cst 3) (Cst 5)) env == Right 15)
  testevalErr14 = ("testevalErr14", 
    evalErr (Sum "x" (Cst 0) (Add (Cst 2) (Cst 2)) (Mul (Var "x") (Var "x"))) env == Right 30)
  testevalErr15 = ("testevalErr15", 
    evalErr (Div (Cst 12) (Cst 0)) env  == Left EDivZero)
  testevalErr16 = ("testevalErr16", evalErr (Var "c") env == Left (EBadVar "c"))
  testevalErr17 = ("testevalErr17", 
    evalErr (Sum "z" (Cst (0)) (Div (Cst 3) (Cst 0) ) (Var "z") ) env == Left EDivZero)


  

main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
