module Parser.ParseStatement where

import AST
import Parser.ParseIdentifier
import Parser.ParseStructural


-- | Parses statements ended with ";".
pStatement :: Parser Statement
pStatement = do
    t <- pFullType
    n <- pAnyIdentifier
    _ <- pSymbol ";"
    return (t, n)