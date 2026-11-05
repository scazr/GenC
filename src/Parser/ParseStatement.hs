module Parser.ParseStatement where

import AST
import Parser.ParseIdentifier
import Parser.ParseStructural



-- | Parses statements ended with ";".
pStatement :: Parser (CType, Identifier)
pStatement = do
    t <- pAnyType
    n <- pAnyIdentifier
    _ <- pSymbol ";"
    return (t, n)