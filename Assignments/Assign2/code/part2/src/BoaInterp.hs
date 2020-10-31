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

rangeBoa :: Int -> Int -> Int -> Value
rangeBoa n1 n2 n3
  | (n1>=n2 && n3>0) || (n1<=n2 && n3<0) = ListVal mempty
  | n3<0 = ListVal (fmap (\x -> IntVal x) [n1, n1+n3 .. n2+1])
  | otherwise = ListVal (fmap (\x -> IntVal x) [n1, n1+n3 .. n2-1])

listToString :: [String] -> String
listToString [] = ""
listToString [x] = x
listToString (x:xs) = x ++ ", " ++ listToString xs

parseBoaValue :: Value -> String
parseBoaValue val = case val of
                      NoneVal -> "None"
                      TrueVal -> "True"
                      FalseVal -> "False"
                      IntVal a -> show a
                      StringVal a -> a
                      ListVal [] -> "[]"
                      ListVal xs -> "[" ++ listToString (fmap (parseBoaValue) xs) ++ "]"

toOneMonad :: [Comp a] -> Comp [a]
toOneMonad [] = return []
toOneMonad (x:xs) = do a <- x; b <- (toOneMonad xs); return (a:b) 


truthy :: Value -> Bool
truthy val
  | val==NoneVal || val==(IntVal 0) || val==(FalseVal) || val==StringVal "" || val==ListVal [] = False
  | otherwise = True

operate :: Op -> Value -> Value -> Either String Value
operate Plus x y = safeArithmeticOp x y (+)
operate Minus x y = safeArithmeticOp x y (-)
operate Times x y = safeArithmeticOp x y (*)
operate Mod x y 
  | y==(IntVal 0) = Left "Div by Zero"
  | otherwise = safeArithmeticOp x y (mod)
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
apply "range" argL = case argL of
                [IntVal n1, IntVal n2, IntVal n3] -> return (rangeBoa n1 n2 n3 )
                [IntVal n1, IntVal n2] -> return (rangeBoa n1 n2 1 )
                [IntVal n2] -> return (rangeBoa 0 n2 1 )
                _ -> abort (EBadArg "Inavalid arguments in range function")
apply "print" argL = case argL of
                [] -> do output (""); return NoneVal --Comp (\e -> (Right NoneVal, [""] ))
                _ ->  do output (foldl1 (\x y -> x ++ " " ++ y ) (fmap (parseBoaValue) argL )); return NoneVal          --in (Right NoneVal, [out] ))
apply fname _= abort (EBadFun fname)

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const val) = return val
eval (Var vName) = look vName
eval (Oper op x y) = do a <- eval x
                        b <- eval y
                        case operate op a b of
                          Left er -> abort (EBadArg er)
                          Right val -> return val
eval (Not x) = do a <- eval x
                  case truthy a of
                    True -> return FalseVal
                    False -> return TrueVal
eval (Call fname expL) = undefined
eval (Call fname expL) = let ad= (fmap (eval) expL)
                          in do agL <- (toOneMonad ad); apply fname agL
eval (List expL) = let ad= (fmap (eval) expL)
                      in do agL <- (toOneMonad ad); return (ListVal agL)
eval (Compr x ccl) = undefined





exec :: Program -> Comp ()
exec [] = return ()
exec (x:xs) = case x of
                SDef vName exp -> do v <- eval exp; withBinding vName v (exec xs)
                SExp exp ->  do eval exp; exec xs

execute :: Program -> ([String], Maybe RunError)
execute program = case  runComp (exec program) [] of
                    (Left er, out) -> (out, Just er)
                    (_, out) -> (out, Nothing)