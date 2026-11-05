module ParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import AST
import Parser.ParseStatement (pStatement)

spec :: Spec
spec = describe "pStatement" $ do
    it "parses standard C types" $ do
        parse pStatement "" "int val;" `shouldParse` (CInt, "val")

    it "parses pointer types" $ do
        parse pStatement "" "int* next;" `shouldParse` (CPointer CInt, "next")

    it "fails when semicolon missing" $ do
        parse pStatement "" `shouldFailOn` "int val"