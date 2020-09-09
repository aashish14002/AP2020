-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone


env :: Env
env = (extendEnv "x" 3 (extendEnv "y" 4 initEnv)) --for testing extendEv, evalFull and evalErr



tests :: [(String, Bool)]
tests = [test1, test2, test3, testA1, testA2, testA3,
 testB1, testB2, testB5, testB6, testB7, testB8,
 testC1, testC2, testC3, testC4, testC5, testC6,
 testD2, testD3, testD4, testD5, testD7, testD8, testD9,testD10,testD11,
 testD12, testD14, testD15,testD16, testD17, testD18, testD19, testD20,
 testE1, testE2, testE3, testE4, testE5, testE6, testE7, testE8, testE9,
 testE10,testE11, testE12, testE13, testE14, testE15,testE16, testE17, 
 testE18, testE19, testE20, testE21, testE22, testE23, testE24, testE25 ] where
  test1 = ("test1", evalSimple (Add (Cst 2) (Cst 2)) == 4)
  test2 = ("test2", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42)
  test3 = ("test3", evalErr (Var "x") initEnv == Left (EBadVar "x"))
  --showExp
  testA1 = ("testA1", showExp (Cst (-2)) == "(-2)")
  testA2 = ("testA2", showExp (Cst (2)) == "2")
  testA3 = ("testA3", showExp (Mul (Cst (-2)) (Add (Cst 2) (Cst 4))) == "((-2)*(2+4))")
  -- testA4 = ("testA4", showExp (Var "x") == "Error : Not a valid expression.")
  
  --evalSimple
  testB1 = ("testB1", evalSimple (Cst (-2)) == (-2))
  testB2 = ("testB2", evalSimple (Div (Cst 12) (Cst 24)) == 0)
  --testB3 = ("testB3", evalSimple (Div (Cst 2) (Cst 0)) == "Error divide by zero")
  --testB4 = ("testB4", evalSimple (Pow (Cst 2) (Sub (Cst 1)(Cst 2))) == "Error negative exponent")
  testB5 = ("testB5", evalSimple (Pow (Cst 0) (Cst 0)) == 1)
  testB6 = ("testB6", evalSimple (Pow (Cst 2) (Mul (Cst 0)(Cst 1))) == 1)
  testB7 = ("testB7", evalSimple (Add (Cst 2) (Cst 2)) == 4)
  testB8 = ("testB8", evalSimple (Sub (Cst 2) (Cst 2)) == 0)
  --testB9 = ("testB9", evalSimple (Var "x") == "Not defined")

  --extendEnv
  testC1 = ("testC1", (extendEnv "x" 3 initEnv) "x" == Just 3)
  testC2 = ("testC2", (extendEnv "y" 4 initEnv) "y" == Just 4)
  testC3 = ("testC3", (extendEnv "y" 4 initEnv) "z" == Nothing)
  testC4 = ("testC4", env "x" == Just 3)
  testC5 = ("testC5", env "y" == Just 4)
  testC6 = ("testC6", env "z" == Nothing)

  --evalFull
  --testD1 = ("testD1", evalFull (Pow (Cst 2) (Sub (Cst 1)(Cst 2))) env == "Error negative exponent")
  testD2 = ("testD2", evalFull (Pow (Cst 0) (Cst 0)) env == 1)
  testD3 = ("testD3", evalFull (Pow (Cst 2) (Mul (Cst 0)(Cst 1))) env == 1)
  testD4 = ("testD4", evalFull (Add (Cst 2) (Cst 2)) env == 4)
  testD5 = ("testD5", evalFull (Sub (Cst 2) (Cst 2)) env == 0)
  --testD6 = ("testD6", evalFull (Div (Cst 2) (Cst 0)) env  == "Error divide by zero")
  testD7 = ("testD7", evalFull (Div (Cst 2) (Cst (-4))) env  == -1)
  testD8 = ("testD8", evalFull (Let "x" (Div (Cst 3) (Cst 0)) (Var "y")) env  == 4)
  testD9 = ("testD9", evalFull (Let "x" (Pow (Cst 3) (Cst 2)) (Add (Var "x")(Var "y"))) env  == 13)
  testD10 = ("testD10", evalFull (Let "x" (Div (Cst 3) (Cst 3)) (Sub (Var "y")(Var "y"))) env  == 0)
  testD11 = ("testD11", evalFull (Let "x" (Cst 5) (Add (Let "x" (Add (Cst 3) (Cst 4)) (Mul (Var "x") (Var "x"))) (Var "x"))) env == 54)
  testD12 = ("testD12", evalFull (Var "x") env == 3)
  --testD13 = ("testD13", evalFull (Var "z") env == "Bad Variable")
  testD14 = ("testD14", evalFull (Var "x") env == 3)
  testD15 = ("testD15", evalFull (If (Cst 0) (Cst 1) (Cst 2)) env == 2)
  testD16 = ("testD16", evalFull (If (Cst (-1)) (Cst 1) (Cst 2)) env == 1)
  testD17 = ("testD17", evalFull (If (Cst 2) (Cst 1) (Cst 2)) env == 1)
  testD18 = ("testD18", evalFull (If (Cst 1) (Cst 1) (Div (Cst 2) (Cst 0))) env == 1)
  testD19 = ("testD19", evalFull (Sum "x" (Cst 1) (Cst 3) (Cst 5)) env == 15)
  testD20 = ("testD20", evalFull (Sum "x" (Cst 5) (Cst 3) (Cst 5)) env == 0)

  --evalErr
  testE1 = ("testE1", evalErr (Pow (Cst 2) (Sub (Cst 1)(Cst 2))) env == Left ENegPower)
  testE2 = ("testE2", evalErr (Pow (Cst 0) (Cst 0)) env == Right 1)
  testE3 = ("testE3", evalErr (Pow (Cst 2) (Mul (Cst 0)(Cst 1))) env == Right 1)
  testE4 = ("testE4", evalErr (Add (Cst 2) (Cst 2)) env == Right 4)
  testE5 = ("testE5", evalErr (Sub (Cst 2) (Cst 2)) env == Right 0)
  testE6 = ("testE6", evalErr (Div (Cst 2) (Cst 0)) env  == Left EDivZero)
  testE7 = ("testE7", evalErr (Div (Cst 2) (Cst (-4))) env  == Right (-1))
  testE8 = ("testE8", evalErr (Let "x" (Div (Cst 3) (Cst 0)) (Var "y")) env  == Left EDivZero)
  testE9 = ("testE9", evalErr (Let "x" (Pow (Cst 3) (Cst 2)) (Add (Var "x")(Var "y"))) env  == Right 13)
  testE10 = ("testE10", evalErr (Let "x" (Div (Cst 3) (Cst 3)) (Sub (Var "y")(Var "y"))) env  == Right 0)
  testE11 = ("testE11", evalErr (Let "x" (Cst 5) (Add (Let "x" (Add (Cst 3) (Cst 4)) (Mul (Var "x") (Var "x"))) (Var "x"))) env == Right 54)
  testE12 = ("testE12", evalErr (Var "x") env == Right 3)
  testE13 = ("testE13", evalErr (Var "z") env == Left (EBadVar "z"))
  testE14 = ("testE14", evalErr (Var "x") env == Right 3)
  testE15 = ("testE15", evalErr (If (Cst 0) (Cst 1) (Cst 2)) env == Right 2)
  testE16 = ("testE16", evalErr (If (Cst (-1)) (Cst 1) (Cst 2)) env == Right 1)
  testE17 = ("testE17", evalErr (If (Cst 2) (Cst 1) (Cst 2)) env == Right 1)
  testE18 = ("testE18", evalErr (If (Cst 1) (Cst 1) (Div (Cst 2) (Cst 0))) env == Right 1)
  testE19 = ("testE19", evalErr (Sum "x" (Cst 1) (Cst 3) (Cst 5)) env == Right 15)
  testE20 = ("testE20", evalErr (Sum "x" (Cst 5) (Cst 3) (Cst 5)) env == Right 0)
  testE21 = ("testE21", evalErr (Sum "z" (Cst (-1)) (Cst 4) (Div (Var "z") (Var"z"))) env == Left EDivZero)
  testE22 = ("testE22", evalErr (Sum "z" (Cst (-1)) (Cst 4) (Div (Var "a") (Var"z"))) env == Left (EBadVar "a"))
  testE23 = ("testE23", evalErr (Sum "z" (Cst (0)) (Cst 0) (Mul (Var "z") (Var"z"))) env == Right 0)
  testE24 = ("testE24", evalErr (Sum "z" (Cst (-1)) (Div (Cst 3) (Cst 2) ) (Mul (Var "z") (Var"z"))) env == Right 2)
  testE25 = ("testE25", evalErr (Sum "z" (Cst (-1)) (Div (Cst 3) (Cst 0) ) (Mul (Var "z") (Var"z"))) env == Left EDivZero)


main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
