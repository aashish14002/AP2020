-- Put your Parser implmenetation in this file.
module ParserImpl where

import Types
-- probably more imports here
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative ((<|>))

type Parser a = ReadP a 

resKeywords = ["and", "false", "if", "implies", "in", "is", "not", "or", "true", "unless"]

tokenSpaceAndComment :: Parser a -> Parser a
tokenSpaceAndComment a = do
                            skipSpaces
                            p <- a
                            skipSpaces
                            return p

parseString :: String -> Either ErrMsg Program
parseString pgrm = case readP_to_S program pgrm of
                    [] -> Left (EUser "no Parse")
                    x -> case last x of
                              (e, "") -> Right e
                              _ -> Left (EInternal "no Parse")

program :: Parser Program
program = tokenSpaceAndComment (do
                                    r <- rule
                                    skipComments
                                    char '.'
                                    skipComments
                                    p <- program
                                    return (r:p)
                                <|> return [])

rule :: Parser Rule
rule = tokenSpaceAndComment (do
                                a <- atom
                                rule' a)

rule' :: Atom -> Parser Rule
rule' atm = tokenSpaceAndComment (do 
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
                                    <|> return (Rule atm CTrue))

cond :: Parser Cond
cond = tokenSpaceAndComment (do
                                f <- condOr
                                condImplies f)

condImplies :: Cond -> Parser Cond
condImplies cnd = tokenSpaceAndComment (do
                                            skipComments
                                            string "implies"
                                            skipComments
                                            c <- cond
                                            return (COr (CNot cnd) c)
                                        <|> return cnd)

condOr :: Parser Cond
condOr = tokenSpaceAndComment (do
                                    f <- condAnd
                                    condOr' f)

condOr' :: Cond -> Parser Cond
condOr' cnd = tokenSpaceAndComment (do
                                        skipComments
                                        string "or"
                                        skipComments
                                        f <- condAnd
                                        f' <- condOr' f
                                        return (COr cnd f')
                                    <|> return cnd)

condAnd :: Parser Cond
condAnd = tokenSpaceAndComment (do
                                    f <- condNot
                                    condAnd' f)

condAnd' :: Cond -> Parser Cond
condAnd' cnd = tokenSpaceAndComment (do
                                        skipComments
                                        string "and"
                                        skipComments
                                        f <- condNot
                                        f' <- condAnd' f
                                        return (CAnd cnd f')
                                    <|> return cnd)

condNot :: Parser Cond
condNot = tokenSpaceAndComment (do
                                    condParen
                                <|> 
                                do
                                    skipComments
                                    string "not"
                                    skipComments
                                    f <- condNot
                                    return (CNot f))

condParen :: Parser Cond
condParen = tokenSpaceAndComment (do
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
                                    return cnd)

condAtom :: Parser Cond
condAtom = tokenSpaceAndComment (do
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
                                    return (CNot CTrue))

condTerm :: Parser Cond
condTerm = tokenSpaceAndComment (do
                                    t <- term 
                                    skipComments
                                    string "is"
                                    skipComments
                                    condTerm' t)

condTerm' :: Term -> Parser Cond
condTerm' trm = tokenSpaceAndComment (do
                                        t <- term
                                        return (CEq trm t)
                                    <|>
                                    do
                                        skipComments
                                        string "not"
                                        skipComments
                                        t <- term
                                        return (CNot (CEq trm t)))

atom :: Parser Atom
atom = tokenSpaceAndComment (do
                                p <- pName
                                skipComments
                                char '('
                                skipComments
                                tz <- termz
                                skipComments
                                char ')'
                                skipComments
                                return (Atom p tz))

termz :: Parser [Term]
termz = tokenSpaceAndComment (do
                                terms
                            <|> return [])

terms :: Parser [Term]
terms = tokenSpaceAndComment (do
                                t <- term
                                terms' t)

terms' :: Term -> Parser [Term]
terms' trm = tokenSpaceAndComment (do
                                    skipComments
                                    char ','
                                    skipComments
                                    t <- terms
                                    return (trm:t)
                                <|>
                                    return [trm])

term :: Parser Term
term = tokenSpaceAndComment (do
                                v <- vName
                                return (TVar v)
                            <|>
                            do
                                d <- constData
                                return (TData d))

pName :: Parser PName
pName = tokenSpaceAndComment name

vName :: Parser VName
vName = tokenSpaceAndComment name

constData :: Parser Data
constData = tokenSpaceAndComment (do
                                    skipComments
                                    char '\"'
                                    c <- pData
                                    char '\"'
                                    skipComments
                                    return c)

pData :: Parser String
pData = tokenSpaceAndComment (do
                                d <- doubleQuotes
                                t <- pData
                                return (d ++ t)
                            <|>
                            do
                                d <- satisfy (\c -> isPrint c && c /= '\"')
                                t <- pData
                                return (d:t )
                            <|>
                                return "")


doubleQuotes :: Parser String
doubleQuotes = tokenSpaceAndComment (do
                                        char '\"'
                                        char '\"'
                                        return "\"")

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
