module Main where

import Prettyprinter (vsep, pretty)
import Prettyprinter.Render.String (renderString)
import Prettyprinter.Internal (layoutPretty, defaultLayoutOptions)
import System.Environment
import System.Process
import Text.Megaparsec (errorBundlePretty, parse)

import Parser.ParseProgram


main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> compile inputFile outputFile
        _ -> putStrLn "Usage: Transpiler <input.gc> <output.exe>"

compile :: FilePath -> FilePath -> IO ()
compile inputFile outputFile = do
    sourceCode <- readFile inputFile

    let ast = case parse pProgram inputFile sourceCode of
            Left err -> error $ "Parse error: " ++ errorBundlePretty err
            Right val -> val

    let compiledDoc = vsep (map pretty ast)

    let cCode = renderString (layoutPretty defaultLayoutOptions compiledDoc)

    putStrLn cCode

    -- let transpiledAst = runMonomorphization ast

    -- let cCode = generateC transpiledAst
    -- let tempCFile = inputFile ++ ".temp.c"
    -- writeFile tempCFile cCode

    -- putStrLn "Compiling with gcc..."
    -- callProcess "gcc" [tempCFile, "-o", outputFile, "-Wall", "-O2"]

    -- removeFile tempCFile
    -- putStrLn $ "Successfully built " ++ outputFile

compileDirectToGcc :: String -> FilePath -> IO ()
compileDirectToGcc cCode outputFile = do
    -- gcc -xc - -o outputFile -Wall
    output <- readProcess "gcc" ["-xc", "-", "-o", outputFile, "-Wall"] cCode
    putStrLn output

