module Parser.ParseIdentifier where

import Text.Megaparsec
import Text.Megaparsec.Char

import AST
import Parser.ParseJunk
import Parser.ParseStructural
import Control.Monad (void)


-- | Parses any user-defined name: Variable names, custom types, typedef declarations, etc.
pAnyIdentifier :: Parser String
pAnyIdentifier = lexeme $ (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')

-- | Parses a specific user-defined name: Variable names, custom types, typedef declarations, etc.
pIdentifier :: String -> Parser String
pIdentifier identifier = lexeme $ string identifier

typeKeywords :: [String]
typeKeywords = [ 
    "int", "char", "float", "double", "void",
    "short", "long", "signed", "unsigned",
    "const", "volatile", "restrict", "_Atomic",
    "struct", "union", "enum"
    ]

resolveSoup :: [String] -> FullType
resolveSoup identifier = BasicType qualifiers base
    where
        isQualifier w = w `elem` ["const", "volatile", "restrict", "_Atomic"]

        qualifiers = filter isQualifier identifier
        typeWords = filter (not . isQualifier) identifier

        base = case typeWords of
            [] -> "int"
            ws -> unwords ws

pTypeWord :: Parser String
pTypeWord = try $ do
    w <- pAnyIdentifier
    if w `elem` typeKeywords
        then return w
        else do
            lookAhead (void (try pAnyIdentifier) <|> void (pSymbol "*"))
            return w

pBasicType :: Parser FullType
pBasicType = do
    soup <- some pTypeWord
    return $ resolveSoup soup

typeQualifiers :: [String]
typeQualifiers =
    ["const", "volatile", "restrict", "_Atomic", "unsigned", "signed", "short", "long", "struct", "union", "enum"]

pQualifier :: Parser QualifierType
pQualifier = choice (map (try . pSymbol) typeQualifiers)

pPointerChunk :: Parser [QualifierType]
pPointerChunk = do
    _ <- pSymbol "*"
    many (choice (map (try . pSymbol) ["const", "volatile", "restrict", "_Atomic"]))

pFullType :: Parser FullType
pFullType = do
    base <- pBasicType
    ptrChunks <- many pPointerChunk
    return $ foldl (flip DerivedPointer) base ptrChunks