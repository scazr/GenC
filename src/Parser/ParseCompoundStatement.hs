module Parser.ParseCompoundStatement where

import Text.Megaparsec

import AST
import Parser.ParseJunk
import Parser.ParseIdentifier
import Parser.ParseRawC
import Parser.ParseStatement
import Parser.ParseStructural
import Parser.ParseFunction

-- | When expecting declaration, try to parse generic block or raw C.
pCompoundStatement :: Parser TemplateDefinition
pCompoundStatement = try pAnyGenericBlock <|> pRawC

-- | When inside generic block, try to parse generic block, a typedef struct or raw C.
pInnerCompoundStatement :: Parser TemplateDefinition
pInnerCompoundStatement = try pAnyGenericBlock <|> try pAnyTypedefStruct <|> try pAnyFunction

-- | Parses C struct block.
pAnyTypedefStruct :: Parser TemplateDefinition
pAnyTypedefStruct = --dbg "TYPEDEF" $ 
    do
    _ <- pSymbol "typedef"
    _ <- pSymbol "struct"
    name <- pAnyIdentifier
    _ <- pSymbol "{"
    fields <- many pStatement
    pJunk
    _ <- pSymbol "}"
    _ <- pIdentifier name
    _ <- pSymbol ";"
    return $ TypedefStruct name fields

-- | Parses a generic block.
pAnyGenericBlock :: Parser TemplateDefinition
pAnyGenericBlock = -- dbg "GENERIC" $ 
    do
    _ <- pSymbol "generic"
    typeVars <- pAnyIdentifier `sepBy1` pSymbol ","
    _ <- pSymbol "{"
    decls <- many pInnerCompoundStatement
    pJunk
    _ <- pSymbol "}"
    return $ GenericBlock typeVars decls