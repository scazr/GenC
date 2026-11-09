module ParserFunctionSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import AST
import Parser.ParseFunction (pAnyFunction)
import Parser.ParseStatement

spec :: Spec
spec = describe "pAnyFunction" $ do
    it "parses function with nothing" $ do
        parse pAnyFunction "" "int test() {}" `shouldParse` Function (BasicType [] "int") "test" [] ""

    it "parses function with one argument a" $ do
        parse pAnyFunction "" "int test(int a) {}" `shouldParse` Function (BasicType [] "int") "test" [(BasicType [] "int", "a")] ""

    it "parses function with body" $ do
        parse pAnyFunction "" "int test() { int x; }" `shouldParse` Function (BasicType [] "int") "test" [] " int x; "

    it "parses function with one argument and body" $ do
        parse pAnyFunction "" "int test(int a) { int x; }" `shouldParse` Function (BasicType [] "int") "test" [(BasicType [] "int", "a")] " int x; "

    it "parses function with two argument and body" $ do
        parse pAnyFunction "" "int test(int a, int b) { int x; }" `shouldParse` Function (BasicType [] "int") "test" [(BasicType [] "int", "a"), (BasicType [] "int", "b")] " int x; "

    it "parses function with weird argument format and body" $ do
        parse pAnyFunction "" (
            "int test(" ++
                "   int a," ++
                "   int b" ++
                ")" ++
                "{" ++ 
                "   int x;" ++ 
                "}"
            ) `shouldParse` Function (BasicType [] "int") "test" [(BasicType [] "int", "a"), (BasicType [] "int", "b")] "   int x;"
        