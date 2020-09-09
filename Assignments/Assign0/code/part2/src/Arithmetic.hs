-- This is a skeleton file for you to edit

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

---------------------------------------
-- error constants
---------------------------------------
invalidExpressionError = "EOther : Not a valid expression"
divZeroError = "EDivZero"
negPowerError = "ENegPower"
badVarError = "EBadVar"

---------------------------------------
-- showExp
---------------------------------------
showExp :: Exp -> String
showExp (Cst x)
  | x < 0 = "("++ show x ++ ")" 
  | otherwise = show x
showExp (Add x y) = "("++ showExp x ++ "+" ++ showExp y ++ ")"
showExp (Sub x y) = "("++ showExp x ++ "-" ++ showExp y ++ ")"
showExp (Mul x y) = "("++ showExp x ++ "*" ++ showExp y ++ ")"
showExp (Div x y) = "("++ showExp x ++ "/" ++ showExp y ++ ")"
showExp (Pow x y) = "("++ showExp x ++ "^" ++ showExp y ++ ")"
showExp _ = error invalidExpressionError

---------------------------------------
-- evalSimple
---------------------------------------
evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add x y) = evalSimple x + evalSimple y
evalSimple (Sub x y) = evalSimple x - evalSimple y
evalSimple (Mul x y) = evalSimple x * evalSimple y
evalSimple (Div x y)
  | (evalSimple y) == 0 = error divZeroError
  | otherwise = div (evalSimple x) (evalSimple y)
evalSimple (Pow x y)
  | (evalSimple y) < 0 = error negPowerError
  ---------------------------------------
  -- evaluate x for errors
  ---------------------------------------
  | (evalSimple x) == 0 = 0 ^ (evalSimple y) 
  | otherwise = (evalSimple x) ^ (evalSimple y)
evalSimple _ = error invalidExpressionError

---------------------------------------
-- extendEnv
---------------------------------------
extendEnv :: VName -> Integer -> Env -> Env
extendEnv name num env e
 |name == e = Just num
 |otherwise = env e

---------------------------------------
-- evalFull
---------------------------------------
evalFull :: Exp -> Env -> Integer
evalFull (Cst x) _ = x
evalFull (Var x) env = case (env x) of 
  Nothing -> error badVarError
  Just v -> v
evalFull (Add x y) env = evalFull x env + evalFull y env
evalFull (Sub x y) env = evalFull x env - evalFull y env
evalFull (Mul x y) env = evalFull x env * evalFull y env
evalFull (Div x y) env
  | (evalFull y env) == 0 = error divZeroError
  | otherwise = div (evalFull x env) (evalFull y env)
evalFull (Pow x y) env
  | (evalFull y env) < 0 = error negPowerError
  | (evalFull x env) == 0 = 0 ^ (evalFull y env)
  | otherwise = (evalFull x env) ^ (evalFull y env)
evalFull (If test yes no) env
  | r == 0 = evalFull no env
  | otherwise = evalFull yes env
  where r = (evalFull test env)
evalFull (Let var aux body) env = evalFull body (extendEnv var x env)
  where x = evalFull aux env
evalFull (Sum var from to body) env
  | n1 > n2 = 0
  | otherwise = (evalFull body env1) + 
        evalFull (Sum var (Add (Cst n1) (Cst 1) ) (Cst n2) body) env 
  where n1 = evalFull from env
        n2 = evalFull to env
        env1 = extendEnv var n1 env

----------------------------------------------------------------
-- evalErrHelp
-- helper function for evalErr which evaluates an expression 
-- and then apply its value to the partial function passed 
-- as parameter if it doesn't retutn ArithError
----------------------------------------------------------------
evalErrHelp :: Exp -> Env -> (Integer -> Either ArithError Integer) -> 
  Either ArithError Integer
evalErrHelp ex env f = case evalErr ex env of
  Left n -> Left n
  Right n -> f n

----------------------------------------------------------------
-- evalErrHelp1
-- helper function for evalErr which evaluates expressions 
-- ex and ex1 and then apply their values to the partial 
-- function passed as parameter if they donot return 
-- ArithError
----------------------------------------------------------------
evalErrHelp1 :: Exp -> Exp -> Env -> Env -> 
  ((Integer, Integer) -> Either ArithError Integer) -> 
  Either ArithError Integer
evalErrHelp1 ex ex1 env env1 f = case evalErr ex env of
  Left n1 -> Left n1
  Right n1 -> case evalErr ex1 env1 of 
    Left n2 -> Left n2
    Right n2 -> f (n1,n2)

---------------------------------------
-- evalErr
---------------------------------------
evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst x) _ = Right x
evalErr (Var x) env = case (env x) of 
  Nothing -> Left (EBadVar x) 
  Just v -> Right v
evalErr (Add x y) env = evalErrHelp1 x y env env f1
  where f1 = \(n1, n2) ->  Right (n1 + n2)
evalErr (Sub x y) env = evalErrHelp1 x y env env f1
  where f1 = \(n1, n2) ->  Right (n1 - n2)
evalErr (Mul x y) env = evalErrHelp1 x y env env f1
  where f1 = \(n1, n2) ->  Right (n1 * n2)
evalErr (Div x y) env = evalErrHelp1 x y env env f1
  where f1 = \(n1, n2) ->  if n2==0 then Left EDivZero else Right (div n1 n2)
evalErr (Pow x y) env = evalErrHelp1 x y env env f1
  where f1 = \(n1, n2) ->  if n2<0 then Left ENegPower else Right (n1 ^ n2)
evalErr (If test yes no) env = evalErrHelp test env f1
  where f1 = \n1 -> if n1==0 then evalErr no env else evalErr yes env
evalErr (Let var aux body) env = evalErrHelp aux env f1
  where f1 = \n1 -> evalErr body (extendEnv var n1 env) 
---------------------------------------------
-- evaluate from and to values and 
-- then while from <= to bind updated 
-- from value in env and compute the
-- sum recursively
---------------------------------------------
evalErr (Sum var from to body) env = evalErrHelp1 from to env env f1  
  where f1 = \(n1, n2) -> if n1>n2 then Right 0 else evalErrHelp1 body (Sum var (Add (Cst n1) (Cst 1) ) (Cst n2) body) (extendEnv var n1 env) env f2
        f2 = \(n3,n4) -> Right (n3 + n4)
 
-- optional parts (if not attempted, leave them unmodified)
---------------------------------------
-- showCompact
---------------------------------------
showCompact :: Exp -> String
showCompact = undefined

---------------------------------------
-- evalEager
---------------------------------------
evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

---------------------------------------
-- evalLazy
---------------------------------------
evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
