-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp (\e -> (Right a, mempty))
  m >>= f = Comp (\e -> case runComp m e of
                          (Left er, xs) -> (Left er, xs)
                          (Right a, xs) -> case runComp (f a) e of
                                             (Left er1, xs1) -> (Left er1, xs++xs1)
                                             (Right a1, xs1) -> (Right a1, xs++xs1))

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort re = Comp (\e -> (Left re, mempty))

look :: VName -> Comp Value
look v = Comp (\e -> case isMemberE v e of
                      Nothing -> (Left (EBadVar v), mempty)
                      Just (v, val) -> (Right val, mempty))

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding v val m = Comp (\e -> runComp m ((v,val):e))

output :: String -> Comp ()
output s = Comp (\e -> (Right (), [s]))

-- Helper functions for interpreter

isMemberE :: VName -> Env -> Maybe (VName, Value)
isMemberE v e = case filter (\(n, _) -> n==v) e of
                  [] -> Nothing
                  xs -> Just (head xs)

safeArithmeticOp :: Value -> Value -> (Int -> Int -> Int) -> Either String Value
safeArithmeticOp x y f = case (x, y) of
                      (IntVal a, IntVal b) -> Right (IntVal (f a b))
                      _ -> Left "EBadArg"

safeComparisonOp :: Value -> Value -> (Int -> Int -> Bool) -> Either String Value
safeComparisonOp x y f = case (x, y) of
                      (IntVal a, IntVal b) -> if (f a b) then (Right TrueVal) else (Right FalseVal)
                      _ -> Left "EBadArg"

truthy :: Value -> Bool
truthy val
  | val==NoneVal || val==(IntVal 0) || val==(FalseVal) || val==StringVal "" || val==ListVal [] = False
  | otherwise = True

operate :: Op -> Value -> Value -> Either String Value
operate Plus x y = safeArithmeticOp x y (+)
operate Minus x y = safeArithmeticOp x y (-)
operate Times x y = safeArithmeticOp x y (*)
operate Mod x y = safeArithmeticOp x y (mod)
operate Div x y
  | y==(IntVal 0) = Left "Div by Zero"
  | otherwise = safeArithmeticOp x y (div)
operate Eq x y = if x==y then (Right TrueVal) else (Right FalseVal)
operate Less x y = safeComparisonOp x y (<) 
operate Greater x y = safeComparisonOp x y (>)
operate In a s = case s of
                  ListVal [] -> (Right FalseVal)
                  ListVal (x:xs) -> if (operate Eq a x) == (Right TrueVal) then (Right TrueVal) else (operate In a (ListVal xs))
                  _ -> Left "EBadArg"

apply :: FName -> [Value] -> Comp Value
apply = undefined

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval = undefined

exec :: Program -> Comp ()
exec = undefined

execute :: Program -> ([String], Maybe RunError)
execute = undefined
