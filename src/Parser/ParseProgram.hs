module Parser.ParseProgram where

import Text.Megaparsec

import AST
import Parser.ParseCompoundStatement
import Parser.ParseJunk


-- | Parse whole file
pProgram :: Parser Program
pProgram = between pJunk eof (many pCompoundStatement)