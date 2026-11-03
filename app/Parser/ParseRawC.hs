module Parser.ParseRawC where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug (dbg)

import AST
import Parser.ParseJunk 
import Parser.ParseStructural

-- | Parse pure C code outside generic blocks.
pRawC :: Parser Declaration
pRawC = do
    notFollowedBy stopCondition
    rawText <- someTill anySingle stopCondition
    return $ RawC rawText
    where
        stopCondition = eof <|> lookAhead (try $ do
            pJunk 
            void (pKeyword "generic") -- <|> void (keyword "typedef" <* space1 <* string "struct")
            )

-- | Parse pure C code inside generic blocks.
pRawInnerC :: Parser Declaration
pRawInnerC = --dbg "RAWC" $
    do 
    notFollowedBy stopCondition
    rawText <- someTill anySingle stopCondition
    return $ RawC rawText
    where
        stopCondition = lookAhead (try $ do
            pJunk
            void (char '}') <|> void (pKeyword "generic")  <|> void (string "typedef" <* space1 <* string "struct")
            )