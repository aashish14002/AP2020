module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   E  ::= T E' | "-" T  E'
--   E' ::= "+" T E' | "-" T E' | e
--   T  ::= num | "(" E ")" 

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)


e :: Parser Exp
e = do a <- t; e' a 
      <|> do skipSpaces; char '-'; a <- t; f <- e' a; return (Negate f)

e' :: Exp -> Parser Exp
e' exp =  do skipSpaces; char '+'; a <- t; f <- e' a; return (Add exp f)
            <|> do skipSpaces; char '-'; a <- t; f <- e' a; return (Add exp (Negate f))
            <|> return exp

t :: Parser Exp
t = do skipSpaces; n <- num; return (Num n)
      <|> do skipSpaces; char '('; skipSpaces; a <- e; skipSpaces; char ')'; skipSpaces; return a


num :: Parser Int
num = do skipSpaces; s<-(munch1 (\c -> c `elem` "0123456789")); skipSpaces; return (read s)   

parseString :: String -> Either ParseError Exp
parseString stmt = case readP_to_S e stmt of
                    [] -> Left "Error"
                    x -> case last x of
                              (e, "") -> Right e
                              _ -> Left "error1"



