-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
-- add any other other imports you need

type ParseError = String -- you may replace this
type Parser a = ReadP a  



parseString :: String -> Either ParseError Program
parseString = undefined  -- define this
