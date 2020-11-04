-- Put your Preprocessor implementation in this file.
module PreprocessorImpl where

import Types
-- Probably more imports here
-- import Control.Mona
clausify :: Program -> Either ErrMsg IDB
clausify program = if (verifyClauses cL) then Right (IDB (ruleToPspec cL) cL ) else (Left (EUser "error"))
                    where cL = (ruleToCLause program)

stratify :: IDB -> [PSpec] -> Either ErrMsg [[PSpec]]
stratify = undefined



-- type Env = [Clause]

-- -- data RunError = EBadVar VName | EBadFun FName | EBadArg String
-- --   deriving (Eq, Show)

-- newtype Comp a = Comp {runComp :: Env -> (Either ErrMsg a) }

-- instance Monad Comp where
--   return a = Comp (\_ -> (Right a))
--   m >>= f = Comp (\e -> case runComp m e of
--                           (Left er) -> (Left er)
--                           (Right a) -> runComp (f a) e
-- -- -- You shouldn't need to modify these
-- instance Functor Comp where
--   fmap = liftM
-- instance Applicative Comp where
--   pure = return; (<*>) = ap

-- -- Operations of the monad
-- abort :: RunError -> Comp a
-- abort re = Comp (\_ -> (Left re))

-- look :: Comp Value
-- look = Comp (\e -> case isMemberE v e of
--                       Nothing -> (Left (EUser v))
--                       Just (_, val) -> (Right val))

-- withBinding :: PName -> [VName] -> Comp a -> Comp a
-- withBinding v val m = Comp (\e -> runComp m ((v,val):e))


data PTest =
    PT Atom
 | PTNot Atom
 | PTEq Term Term
 | PTNeq Term Term
 deriving (Eq, Show)

data PClause = PClause [Atom] [Test]
  deriving (Eq, Show)

evaluate :: Cond -> [[PTest]]
evaluate (CAtom a) = [[(PT a)]]
evaluate (CEq c1 c2) = [[(PTEq c1 c2)]]
evaluate CTrue = [[]]
evaluate (CNot a) = case a of
                    (CAnd c1 c2) -> evaluate (CNot c1) ++ evaluate (CNot c2)
                    (COr c1 c2) -> evaluate (CAnd (CNot c1) (CNot c2) )
                    (CNot c1) -> evaluate c1
                    (CAtom a) -> [[(PTNot a)]]
                    CTrue -> [[]]
                    (CEq c1 c2) -> [[(PTNeq c1 c2)]]
evaluate (CAnd c1 (CNot CTrue)) = [[]]
evaluate (CAnd c1 (COr c2 c3 ) ) = evaluate (CAnd c1 c2) ++ evaluate (CAnd c1 c3)
evaluate (CAnd (COr c2 c3 ) c1) = evaluate (CAnd c1 c2) ++ evaluate (CAnd c1 c3)
evaluate (CAnd c1 c2) = [concat ((evaluate c1) ++ (evaluate c2))]
evaluate (COr c1 c2) = (evaluate c1) ++ (evaluate c2) 

evaluateTest :: [PTest] -> PClause
evaluateTest [] = PClause [] []
evaluateTest (p:pt) = let xs = (evaluateTest pt)
                    in case (p,xs) of
                        ((PT p),(PClause a t))  -> PClause (p:a) t
                        ((PTNot p),(PClause a t)) -> PClause a ((TNot p):t)
                        ((PTEq c1 c2),(PClause a t)) -> PClause a ((TEq c1 c2):t)
                        ((PTNeq c1 c2),(PClause a t)) -> PClause a ((TNeq c1 c2):t)

condToPClause :: Cond -> [PClause]
condToPClause c = fmap (evaluateTest) (evaluate c)

pClauseToClause :: Atom -> PClause -> Clause
pClauseToClause a (PClause atm tst) = Clause a atm tst

getPspec :: Atom -> PSpec
getPspec (Atom pName terms) = (pName, (length terms))

ruleToCLause :: Program -> [Clause]
ruleToCLause [] = []
ruleToCLause ((Rule a c):p) = (fmap (pClauseToClause a) (condToPClause c))++(ruleToCLause p)

ruleToPspec :: [Clause] -> [PSpec]
ruleToPspec [] = []
ruleToPspec ((Clause a aL tL):p) = if (specA `elem` specList) then specList else (specA:specList)
                                    where specA = (getPspec a); specList = (ruleToPspec p)


getVNames :: Atom -> [VName]
getVNames (Atom _ []) = []
getVNames (Atom pName ((TVar t):terms)) = if (t `elem` vList) then vList else (t:vList)
                                    where vList = (getVNames(Atom pName terms))
getVNames (Atom pName ((TData t):terms)) = getVNames(Atom pName terms)

verifyClause :: Clause -> Bool
verifyClause (Clause atm atoms tests) = if (filter (`elem` concat(map getVNames atoms)) atmNames) == atmNames then True else False
                                        where atmNames = (getVNames atm)

verifyClauses :: [Clause] -> Bool
verifyClauses [] = True
verifyClauses (p:pL) = if (verifyClause p) then (verifyClauses pL) else False


