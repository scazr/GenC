module Parser.ParseFunction where

import Text.Megaparsec
import Text.Megaparsec.Char

import AST
import Parser.ParseIdentifier
import Parser.ParseStructural (pSymbol)

-- import Text.Megaparsec.Debug (dbg)
import Parser.ParseJunk (pJunk)

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

        pAnyFunctionBody :: Parser String
        pAnyFunctionBody = do
            _ <- char '{'
            body <- concat <$> many pChunk
            _ <- char '}'
            return body

        pChunk :: Parser String
        pChunk = pBracedBlock <|> pSingleChar

        pBracedBlock :: Parser String
        pBracedBlock = do
            open <- string "{"
            body <- concat <$> many pChunk
            close <- string "}"
            return $ open ++ body ++ close

        pSingleChar :: Parser String
        pSingleChar = do
            notFollowedBy (char '}') 
            c <- anySingle
            return [c]