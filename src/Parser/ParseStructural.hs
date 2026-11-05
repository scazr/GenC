module Parser.ParseStructural where

import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char

import AST
import Parser.ParseJunk

-- | Match exact structural non-alphanumeric like "{", "}", ";", "*", etc.
-- Doesn't care if there's alphanumeric chars after it.
-- Consumes spaces after if there's.
-- Parses the exact string asked and uses pJunk for trailing spaces.
pSymbol :: String -> Parser String
pSymbol = L.symbol pJunk

-- | Match exact structural keywords like "generic".
-- Will not work if there's alphanumeric chars after it like "generic_list".
-- Consumes spaces after if there's.
pKeyword :: String -> Parser String
pKeyword w = try $ do
    _ <- string w
    notFollowedBy (alphaNumChar <|> char '_')
    return w