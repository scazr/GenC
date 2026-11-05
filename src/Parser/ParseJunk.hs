module Parser.ParseJunk where

import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST

-- | Ignores comments and consumes whitespaces.
pJunk :: Parser ()
pJunk = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- | Wrap a parser in lexeme. It will parse the target text and run pJunk.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme pJunk