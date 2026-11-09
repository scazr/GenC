module ParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import AST
import Parser.ParseStatement (pStatement)

spec :: Spec
spec = describe "pStatement" $ do
    it "parses standard C types" $ do
        parse pStatement "" "int val;" `shouldParse` (BasicType [] "int", "val")

    it "parses pointer types" $ do
        parse pStatement "" "int* val;" `shouldParse` (DerivedPointer [] $ BasicType [] "int", "val")

    it "parses pointer types" $ do
        parse pStatement "" "int** val;" `shouldParse` (DerivedPointer [] $ DerivedPointer [] $ BasicType [] "int", "val")

    it "parses pointer types" $ do
        parse pStatement "" "int*** val;" `shouldParse` (DerivedPointer [] $ DerivedPointer [] $ DerivedPointer [] $ BasicType [] "int", "val")

    it "fails when semicolon missing" $ do
        parse pStatement "" `shouldFailOn` "int val"

    it "fails when type missing" $ do
        parse pStatement "" `shouldFailOn` "val;"

    it "fails when name missing" $ do
        parse pStatement "" `shouldFailOn` "int;" 

    it "fails when nothing" $ do
        parse pStatement "" `shouldFailOn` "" 

    it "fails when nothing" $ do
        parse pStatement "" `shouldFailOn` ";" 