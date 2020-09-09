-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone


env :: Env
env = (extendEnv "x" 3 (extendEnv "y" 4 initEnv)) --for testing extendEv, evalFull and evalErr



tests :: [(String, Bool)]
tests = [test1, test2, test3, testShowExp1, testShowExp2, testShowExp3,
 testevalSimple1, testevalSimple2, testevalSimple5, testevalSimple6, testevalSimple7, testevalSimple8,
 testextendEnv1, testextendEnv2, testextendEnv3, testextendEnv4, testextendEnv5, testextendEnv6,
 testevalFull2, testevalFull3, testevalFull4, testevalFull5, testevalFull7, testevalFull8, testevalFull9,testevalFull10,testevalFull11,
 testevalFull12, testevalFull14, testevalFull15,testevalFull16, testevalFull17, testevalFull18, testevalFull19, testevalFull20,
 testevalErr1, testevalErr2, testevalErr3, testevalErr4, testevalErr5, testevalErr6, testevalErr7, testevalErr8, testevalErr9,
 testevalErr10,testevalErr11, testevalErr12, testevalErr13, testevalErr14, testevalErr15,testevalErr16, testevalErr17, 
 testevalErr18, testevalErr19, testevalErr20, testevalErr21, testevalErr22, testevalErr23, testevalErr24, testevalErr25 ] where
  test1 = ("test1", evalSimple (Add (Cst 2) (Cst 2)) == 4)
  test2 = ("test2", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42)
  test3 = ("test3", evalErr (Var "x") initEnv == Left (EBadVar "x"))
  --showExp
  testShowExp1 = ("testShowExp1", showExp (Cst (-2)) == "(-2)")
  testShowExp2 = ("testShowExp2", showExp (Cst (2)) == "2")
  testShowExp3 = ("testShowExp3", showExp (Mul (Cst (-2)) (Add (Cst 2) (Cst 4))) == "((-2)*(2+4))")
  -- testShowExp4 = ("testShowExp4", showExp (Var "x") == "*** Exception: EOther : Not a valid expression")
  
  --evalSimple
  testevalSimple1 = ("testevalSimple1", evalSimple (Cst (-2)) == (-2))
  testevalSimple2 = ("testevalSimple2", evalSimple (Div (Cst 12) (Cst 24)) == 0)
  --testevalSimple3 = ("testevalSimple3", evalSimple (Div (Cst 2) (Cst 0)) == "Error divide by zero")
  --testevalSimple4 = ("testevalSimple4", evalSimple (Pow (Cst 2) (Sub (Cst 1)(Cst 2))) == "Error negative exponent")
  testevalSimple5 = ("testevalSimple5", evalSimple (Pow (Cst 0) (Cst 0)) == 1)
  testevalSimple6 = ("testevalSimple6", evalSimple (Pow (Cst 2) (Mul (Cst 0)(Cst 1))) == 1)
  testevalSimple7 = ("testevalSimple7", evalSimple (Add (Cst 2) (Cst 2)) == 4)
  testevalSimple8 = ("testevalSimple8", evalSimple (Sub (Cst 2) (Cst 2)) == 0)
  --testevalSimple9 = ("testevalSimple9", evalSimple (Var "x") == "Not defined")

  --extendEnv
  testextendEnv1 = ("testextendEnv1", (extendEnv "x" 3 initEnv) "x" == Just 3)
  testextendEnv2 = ("testextendEnv2", (extendEnv "y" 4 initEnv) "y" == Just 4)
  testextendEnv3 = ("testextendEnv3", (extendEnv "y" 4 initEnv) "z" == Nothing)
  testextendEnv4 = ("testextendEnv4", env "x" == Just 3)
  testextendEnv5 = ("testextendEnv5", env "y" == Just 4)
  testextendEnv6 = ("testextendEnv6", env "z" == Nothing)

  --evalFull
  --testevalFull1 = ("testevalFull1", evalFull (Pow (Cst 2) (Sub (Cst 1)(Cst 2))) env == "Error negative exponent")
  testevalFull2 = ("testevalFull2", evalFull (Pow (Cst 0) (Cst 0)) env == 1)
  testevalFull3 = ("testevalFull3", evalFull (Pow (Cst 2) (Mul (Cst 0)(Cst 1))) env == 1)
  testevalFull4 = ("testevalFull4", evalFull (Add (Cst 2) (Cst 2)) env == 4)
  testevalFull5 = ("testevalFull5", evalFull (Sub (Cst 2) (Cst 2)) env == 0)
  --testevalFull6 = ("testevalFull6", evalFull (Div (Cst 2) (Cst 0)) env  == "Error divide by zero")
  testevalFull7 = ("testevalFull7", evalFull (Div (Cst 2) (Cst (-4))) env  == -1)
  testevalFull8 = ("testevalFull8", evalFull (Let "x" (Div (Cst 3) (Cst 0)) (Var "y")) env  == 4)
  testevalFull9 = ("testevalFull9", evalFull (Let "x" (Pow (Cst 3) (Cst 2)) (Add (Var "x")(Var "y"))) env  == 13)
  testevalFull10 = ("testevalFull10", evalFull (Let "x" (Div (Cst 3) (Cst 3)) (Sub (Var "y")(Var "y"))) env  == 0)
  testevalFull11 = ("testevalFull11", evalFull (Let "x" (Cst 5) (Add (Let "x" (Add (Cst 3) (Cst 4)) (Mul (Var "x") (Var "x"))) (Var "x"))) env == 54)
  testevalFull12 = ("testevalFull12", evalFull (Var "x") env == 3)
  --testevalFull13 = ("testevalFull13", evalFull (Var "z") env == "Bad Variable")
  testevalFull14 = ("testevalFull14", evalFull (Var "x") env == 3)
  testevalFull15 = ("testevalFull15", evalFull (If (Cst 0) (Cst 1) (Cst 2)) env == 2)
  testevalFull16 = ("testevalFull16", evalFull (If (Cst (-1)) (Cst 1) (Cst 2)) env == 1)
  testevalFull17 = ("testevalFull17", evalFull (If (Cst 2) (Cst 1) (Cst 2)) env == 1)
  testevalFull18 = ("testevalFull18", evalFull (If (Cst 1) (Cst 1) (Div (Cst 2) (Cst 0))) env == 1)
  testevalFull19 = ("testevalFull19", evalFull (Sum "x" (Cst 1) (Cst 3) (Cst 5)) env == 15)
  testevalFull20 = ("testevalFull20", evalFull (Sum "x" (Cst 5) (Cst 3) (Cst 5)) env == 0)

  --evalErr
  testevalErr1 = ("testevalErr1", evalErr (Pow (Cst 2) (Sub (Cst 1)(Cst 2))) env == Left ENegPower)
  testevalErr2 = ("testevalErr2", evalErr (Pow (Cst 0) (Cst 0)) env == Right 1)
  testevalErr3 = ("testevalErr3", evalErr (Pow (Cst 2) (Mul (Cst 0)(Cst 1))) env == Right 1)
  testevalErr4 = ("testevalErr4", evalErr (Add (Cst 2) (Cst 2)) env == Right 4)
  testevalErr5 = ("testevalErr5", evalErr (Sub (Cst 2) (Cst 2)) env == Right 0)
  testevalErr6 = ("testevalErr6", evalErr (Div (Cst 2) (Cst 0)) env  == Left EDivZero)
  testevalErr7 = ("testevalErr7", evalErr (Div (Cst 2) (Cst (-4))) env  == Right (-1))
  testevalErr8 = ("testevalErr8", evalErr (Let "x" (Div (Cst 3) (Cst 0)) (Var "y")) env  == Left EDivZero)
  testevalErr9 = ("testevalErr9", evalErr (Let "x" (Pow (Cst 3) (Cst 2)) (Add (Var "x")(Var "y"))) env  == Right 13)
  testevalErr10 = ("testevalErr10", evalErr (Let "x" (Div (Cst 3) (Cst 3)) (Sub (Var "y")(Var "y"))) env  == Right 0)
  testevalErr11 = ("testevalErr11", evalErr (Let "x" (Cst 5) (Add (Let "x" (Add (Cst 3) (Cst 4)) (Mul (Var "x") (Var "x"))) (Var "x"))) env == Right 54)
  testevalErr12 = ("testevalErr12", evalErr (Var "x") env == Right 3)
  testevalErr13 = ("testevalErr13", evalErr (Var "z") env == Left (EBadVar "z"))
  testevalErr14 = ("testevalErr14", evalErr (Var "x") env == Right 3)
  testevalErr15 = ("testevalErr15", evalErr (If (Cst 0) (Cst 1) (Cst 2)) env == Right 2)
  testevalErr16 = ("testevalErr16", evalErr (If (Cst (-1)) (Cst 1) (Cst 2)) env == Right 1)
  testevalErr17 = ("testevalErr17", evalErr (If (Cst 2) (Cst 1) (Cst 2)) env == Right 1)
  testevalErr18 = ("testevalErr18", evalErr (If (Cst 1) (Cst 1) (Div (Cst 2) (Cst 0))) env == Right 1)
  testevalErr19 = ("testevalErr19", evalErr (Sum "x" (Cst 1) (Cst 3) (Cst 5)) env == Right 15)
  testevalErr20 = ("testevalErr20", evalErr (Sum "x" (Cst 5) (Cst 3) (Cst 5)) env == Right 0)
  testevalErr21 = ("testevalErr21", evalErr (Sum "z" (Cst (-1)) (Cst 4) (Div (Var "z") (Var"z"))) env == Left EDivZero)
  testevalErr22 = ("testevalErr22", evalErr (Sum "z" (Cst (-1)) (Cst 4) (Div (Var "a") (Var"z"))) env == Left (EBadVar "a"))
  testevalErr23 = ("testevalErr23", evalErr (Sum "z" (Cst (0)) (Cst 0) (Mul (Var "z") (Var"z"))) env == Right 0)
  testevalErr24 = ("testevalErr24", evalErr (Sum "z" (Cst (-1)) (Div (Cst 3) (Cst 2) ) (Mul (Var "z") (Var"z"))) env == Right 2)
  testevalErr25 = ("testevalErr25", evalErr (Sum "z" (Cst (-1)) (Div (Cst 3) (Cst 0) ) (Mul (Var "z") (Var"z"))) env == Left EDivZero)


main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
