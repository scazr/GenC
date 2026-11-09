module Parser.ParseIdentifier where

import Text.Megaparsec
import Text.Megaparsec.Char

import AST
import Parser.ParseJunk
import Parser.ParseStructural


-- | Parses any user-defined name: Variable names, custom types, typedef declarations, etc.
pAnyIdentifier :: Parser String
pAnyIdentifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

-- | Parses a specific user-defined name: Variable names, custom types, typedef declarations, etc.
pIdentifier :: String -> Parser String
pIdentifier identifier = lexeme $ string identifier

typeQualifiers :: [String]
typeQualifiers =
    ["const", "volatile", "restrict", "_Atomic", "unsigned", "signed", "short", "long", "struct", "union", "enum"]

pQualifier :: Parser QualifierType
pQualifier = choice (map (try . pSymbol) typeQualifiers)

pBasicType :: Parser FullType
pBasicType = do
    preQuals <- many pQualifier
    baseName <- pAnyIdentifier
    postQuals <- many pQualifier
    return $ BasicType (preQuals ++ postQuals) baseName

pPointerChunk :: Parser [QualifierType]
pPointerChunk = do
    _ <- pSymbol "*"
    many (choice (map (try . pSymbol) ["const", "volatile", "restrict", "_Atomic"]))

pFullType :: Parser FullType
pFullType = do
    base <- pBasicType
    ptrChunks <- many pPointerChunk
    return $ foldl (flip DerivedPointer) base ptrChunks