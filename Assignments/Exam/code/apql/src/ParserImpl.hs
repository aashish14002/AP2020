-- Put your Parser implmenetation in this file.
module ParserImpl where

import Types
-- probably more imports here
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative ((<|>))

type Parser a = ReadP a 

resKeywords = ["and", "false", "if", "implies", "in", "is", "not", "or", "true", "unless"]


parseString :: String -> Either ErrMsg Program
parseString pgrm = case readP_to_S program pgrm of
                    [] -> Left (EUser "no Parse")
                    x -> case last x of
                              (e, "") -> Right e
                              _ -> Left (EInternal "no Parse")

program :: Parser Program
program = do
            r <- rule
            skipComments
            char '.'
            skipComments
            p <- program
            return (r:p)
        <|> return []

rule :: Parser Rule
rule = do
        a <- atom
        rule' a

rule' :: Atom -> Parser Rule
rule' atm = do 
                skipComments
                string "if"
                skipComments
                c <- cond
                return (Rule atm c)
            <|>
            do 
                skipComments
                string "unless"
                skipComments
                c <- cond
                return (Rule atm (CNot c))
            <|> return (Rule atm CTrue)

cond :: Parser Cond
cond = do
        f <- condOr
        condImplies f

condImplies :: Cond -> Parser Cond
condImplies cnd = do
                    skipComments
                    string "implies"
                    skipComments
                    c <- cond
                    return (COr (CNot cnd) c)
            <|> return cnd

condOr :: Parser Cond
condOr = do
            f <- condAnd
            condOr' f

condOr' :: Cond -> Parser Cond
condOr' cnd = do
                skipComments
                string "or"
                skipComments
                f <- condAnd
                f' <- condOr' f
                return (COr cnd f')
            <|> return cnd

condAnd :: Parser Cond
condAnd = do
            f <- condNot
            condAnd' f

condAnd' :: Cond -> Parser Cond
condAnd' cnd = do
                skipComments
                string "and"
                skipComments
                f <- condNot
                f' <- condAnd' f
                return (CAnd cnd f')
            <|> return cnd

condNot :: Parser Cond
condNot = do
            condParen
        <|> 
        do
            skipComments
            string "not"
            skipComments
            f <- condNot
            return (CNot f)

condParen :: Parser Cond
condParen = do
                condAtom
            <|> 
            do
                skipComments
                char '('
                skipComments
                cnd <- cond
                skipComments
                char ')'
                skipComments
                return cnd

condAtom :: Parser Cond
condAtom = do
                atm <- atom
                return (CAtom atm)
            <|>
            do
                condTerm
            <|>
            do
                skipComments
                string "true"
                skipComments
                return CTrue
            <|>
            do
                skipComments
                string "false"
                skipComments
                return (CNot CTrue)

condTerm :: Parser Cond
condTerm = do
                t <- term 
                skipComments
                string "is"
                skipComments
                condTerm' t

condTerm' :: Term -> Parser Cond
condTerm' trm = do
                    t <- term
                    return (CEq trm t)
                <|>
                do
                    skipComments
                    string "not"
                    skipComments
                    t <- term
                    return (CNot (CEq trm t))

atom :: Parser Atom
atom = do
            p <- pName
            skipComments
            char '('
            skipComments
            tz <- termz
            skipComments
            char ')'
            skipComments
            return (Atom p tz)

termz :: Parser [Term]
termz = do
            terms
        <|> return []

terms :: Parser [Term]
terms = do
            t <- term
            terms' t

terms' :: Term -> Parser [Term]
terms' trm = do
                skipComments
                char ','
                skipComments
                t <- terms
                return (trm:t)
            <|>
                return [trm]

term :: Parser Term
term = do
            v <- vName
            return (TVar v)
        <|>
        do
            d <- constData
            return (TData d)

pName :: Parser PName
pName = name

vName :: Parser VName
vName = name

constData :: Parser Data
constData = do
                skipComments
                char '\"'
                c <- pData
                char '\"'
                skipComments
                return c

pData :: Parser String
pData = do
        d <- doubleQuotes
        t <- pData
        return (d ++ t)
   <|>
    do
        d <- satisfy (\c -> isPrint c && c /= '\"')
        t <- pData
        return (d:t )
    <|>
        return ""


doubleQuotes :: Parser String
doubleQuotes = do
                char '\"'
                char '\"'
                return "\""

name :: Parser String
name = do 
            skipComments
            c <- satisfy isLetter
            cs <- many (satisfy isLetter <|> satisfy isDigit <|> satisfy (\c -> c=='_'))
            skipComments
            if ((c:cs) `elem` resKeywords) then
                pfail
            else return (c:cs)

skipComments :: Parser ()
skipComments = do
                    skipSpaces
                    string "(*"
                    manyTill get (string "*)")
                    skipSpaces
                    skipComments
                <|>
                    return ()
