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

showExp :: Exp -> String
showExp (Cst x)
  | x < 0 = "("++ show x ++ ")" 
  | otherwise = show x
showExp (Add x y) = "("++ showExp x ++ " + " ++ showExp y ++ ")"
showExp (Sub x y) = "("++ showExp x ++ " - " ++ showExp y ++ ")"
showExp (Mul x y) = "("++ showExp x ++ " * " ++ showExp y ++ ")"
showExp (Div x y) = "("++ showExp x ++ " / " ++ showExp y ++ ")"
showExp (Pow x y) = "("++ showExp x ++ " ^ " ++ showExp y ++ ")"
showExp _ = error "Error : Not a valid expression"

evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add x y) = (evalSimple x) + (evalSimple y)
evalSimple (Sub x y) = (evalSimple x) - (evalSimple y)
evalSimple (Mul x y) = (evalSimple x) * (evalSimple y)
evalSimple (Div x y)
  | (evalSimple y)==0 = error "Divide by zero error"
  | otherwise = div (evalSimple x) (evalSimple y)
evalSimple (Pow x y)
  | (evalSimple y)<0 = error "negative power error"
  | (evalSimple x)==0 = 0
  | otherwise = (evalSimple x) ^ (evalSimple y)
evalSimple _ = error "Error : Not a valid expression"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv = undefined

evalFull :: Exp -> Env -> Integer
evalFull = undefined

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr = undefined


-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
