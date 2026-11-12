module Parser.ParseChunk where

import Text.Megaparsec
import Text.Megaparsec.Char

import AST
import Parser.ParseIdentifier


pChunk :: Parser [BodyChunk]
pChunk = (:[])
    <$> pTemplateCall
    <|> pBracedBlock
    <|> pSingleChar

pTemplateCall :: Parser BodyChunk
pTemplateCall = try $ do
    funcName <- pAnyIdentifier
    TemplateCall funcName <$> pTemplateArgs

pBracedBlock :: Parser [BodyChunk]
pBracedBlock = do
    _ <- string "{"
    body <- concat <$> many pChunk
    _ <- string "}"
    return $ [RawBodyText "{"] ++ body ++ [RawBodyText "}"]

pSingleChar :: Parser [BodyChunk]
pSingleChar = do
    notFollowedBy (char '}')
    c <- anySingle
    return [RawBodyText [c]]
    
-- | Helper: Combines consecutive single-character RawBodyText chunks into strings for efficiency
optimizeChunks :: [BodyChunk] -> [BodyChunk]
optimizeChunks [] = []
optimizeChunks (RawBodyText a : RawBodyText b : rest) = 
    optimizeChunks (RawBodyText (a ++ b) : rest)
optimizeChunks (other : rest) = other : optimizeChunks rest