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

-- | Parse "int", "float" or custom typedef keywords.
pAnyBaseType :: Parser CType
pAnyBaseType = (CInt <$ pSymbol "int")
        <|> (CFloat <$ pSymbol "float")
        <|> (CCustom <$> pAnyIdentifier)

-- | Extracts the base type and wraps it in CPointer(s) if pointer(s).
pAnyType :: Parser CType
pAnyType = do
    baseType <- pAnyBaseType
    pointers <- many (pSymbol "*")
    return $ foldl (\t _ -> CPointer t) baseType pointers