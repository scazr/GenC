module Parser.ParseRawC where

import Text.Megaparsec

import AST
import Parser.ParseChunk
import Parser.ParseStructural

-- | Parse pure C code outside generic blocks.
pRawC :: Parser TemplateDefinition
pRawC = do
    chunks <- some $ do 
        notFollowedBy (pSymbol "generic")
        pChunk
    return $ RawC (optimizeChunks (concat chunks))
--   where
--     pCChunk = pBracedBlock <|> pSingleChar

--     pBracedBlock = do
--         open <- string "{"
--         body <- concat <$> many pCChunk
--         close <- string "}"
--         return $ open ++ body ++ close 
    
--     pSingleChar = do
--         notFollowedBy outerStopCondition
--         notFollowedBy (string "}")
--         c <- anySingle
--         return [c]

--     outerStopCondition = eof <|> lookAhead (try $ void (pKeyword "generic"))

-- | Parse pure C code inside generic blocks.
-- pRawInnerC :: Parser TemplateDefinition
-- pRawInnerC = do 
--     notFollowedBy innerStopCondition
--     chunks <- some pCChunk
--     return $ RawC (concat chunks)
--   where
--     pCChunk = pBracedBlock <|> pSingleChar

--     pBracedBlock = do
--         open <- string "{"
--         body <- concat <$> many pCChunk
--         close <- string "}"
--         return $ open ++ body ++ close 

--     pSingleChar = do
--         notFollowedBy innerStopCondition
--         notFollowedBy (string "}")
--         c <- anySingle
--         return [c]

--     innerStopCondition = lookAhead $ try $ void (
--             string "}"
--         <|> pKeyword "generic"
--         <|> (string "typedef" <* space1 <* string "struct")
--         )