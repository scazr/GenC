module Parser.ParseFunction where

import Text.Megaparsec
import Text.Megaparsec.Char

import AST
import Parser.ParseChunk
import Parser.ParseIdentifier
import Parser.ParseStructural (pSymbol)
import Parser.ParseJunk (pJunk)
-- import Text.Megaparsec.Debug (dbg)


-- | Parses function inside generic block
pAnyFunction :: Parser TemplateDefinition
pAnyFunction = --dbg "FUNCTION" $
    do
    returnType <- pFullType
    name <- pAnyIdentifier
    _ <- pSymbol "("
    args <- pArgs `sepBy` pSymbol ","
    _ <- pSymbol ")"
    body <- pAnyFunctionBody
    pJunk
    return $ Function returnType name args body

    where
        pArgs = do
            t <- pFullType
            n <- pAnyIdentifier
            return (t, n)

        pAnyFunctionBody :: Parser [BodyChunk]
        pAnyFunctionBody = do
            _ <- char '{'
            body <- concat <$> many pChunk
            _ <- char '}'
            return $ optimizeChunks body

