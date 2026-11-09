-- To be implemented yet. Huge headache.

-- | Catch preprocessor directives (#include, #define, #ifdef, etc.)
pMacroDirective :: Parser TemplateDefinition
pMacroDirective = do
    _ <- char '#'
    restOfLine <- manyTill anySingle (char '\n')
    pJunk 
    return $ RawC ("#" ++ restOfLine ++ "\n")