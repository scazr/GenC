module ParseIdentifierSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import AST
import Parser.ParseIdentifier
import Parser.ParseStatement

spec :: Spec
spec = describe "pStatement Type Parsing (The Soup)" $ do

    let testParse input expected = 
            parse pStatement "" input `shouldBe` Right expected
    
    describe "1. Basic Primitives" $ do
      it "parses simple int" $ testParse "int x;" (BasicType [] "int", "x")
      it "parses simple char" $ testParse "char c;" (BasicType [] "char", "c")
      it "parses simple float" $ testParse "float f;" (BasicType [] "float", "f")
      it "parses simple double" $ testParse "double d;" (BasicType [] "double", "d")
      it "parses simple void" $ testParse "void v;" (BasicType [] "void", "v")

    describe "2. Multi-word Primitives" $ do
      it "parses unsigned int" $ testParse "unsigned int u;" (BasicType [] "unsigned int", "u")
      it "parses signed char" $ testParse "signed char sc;" (BasicType [] "signed char", "sc")
      it "parses long long" $ testParse "long long ll;" (BasicType [] "long long", "ll")
      it "parses long double" $ testParse "long double ld;" (BasicType [] "long double", "ld")
      it "parses unsigned long int" $ testParse "unsigned long int uli;" (BasicType [] "unsigned long int", "uli")

    describe "3. Standard Qualifiers (Normal Order)" $ do
      it "parses const int" $ testParse "const int ci;" (BasicType ["const"] "int", "ci")
      it "parses volatile float" $ testParse "volatile float vf;" (BasicType ["volatile"] "float", "vf")
      it "parses _Atomic double" $ testParse "_Atomic double ad;" (BasicType ["_Atomic"] "double", "ad")
      it "parses const unsigned long int" $ testParse "const unsigned long int c;" (BasicType ["const"] "unsigned long int", "c")

    describe "4. The 'Soup' (Jumbled / Reversed Order)" $ do
      it "parses int const" $ testParse "int const ic;" (BasicType ["const"] "int", "ic")
      it "parses unsigned const int" $ testParse "unsigned const int uci;" (BasicType ["const"] "unsigned int", "uci")
      it "parses long int const unsigned" $ testParse "long int const unsigned x;" (BasicType ["const"] "long int unsigned", "x")
      it "parses volatile const int" $ testParse "volatile const int vci;" (BasicType ["volatile", "const"] "int", "vci")
      it "parses int _Atomic const" $ testParse "int _Atomic const iac;" (BasicType ["_Atomic", "const"] "int", "iac")

    describe "5. The 'Implicit Int' Edge Cases" $ do
      it "injects int for const" $ testParse "const c;" (BasicType ["const"] "int", "c")
      it "injects int for volatile" $ testParse "volatile v;" (BasicType ["volatile"] "int", "v")
      it "injects int for const volatile" $ testParse "const volatile cv;" (BasicType ["const", "volatile"] "int", "cv")

    describe "6. Structs, Unions, and Enums" $ do
      it "parses struct Node" $ testParse "struct Node n;" (BasicType [] "struct Node", "n")
      it "parses union Data" $ testParse "union Data d;" (BasicType [] "union Data", "d")
      it "parses enum Color" $ testParse "enum Color c;" (BasicType [] "enum Color", "c")
      it "parses const struct Node" $ testParse "const struct Node cn;" (BasicType ["const"] "struct Node", "cn")
      it "parses struct Node const" $ testParse "struct Node const snc;" (BasicType ["const"] "struct Node", "snc")

    describe "7. Custom Types (Typedefs / Template Types)" $ do
      it "parses size_t" $ testParse "size_t s;" (BasicType [] "size_t", "s")
      it "parses custom template T" $ testParse "T val;" (BasicType [] "T", "val")
      it "parses const T" $ testParse "const T cval;" (BasicType ["const"] "T", "cval")
      it "parses T const" $ testParse "T const tval;" (BasicType ["const"] "T", "tval")
      it "parses custom struct List" $ testParse "List l;" (BasicType [] "List", "l")

    describe "8. Pointers (Basic)" $ do
      it "parses int pointer" $ testParse "int * p;" (DerivedPointer [] (BasicType [] "int"), "p")
      it "parses custom T pointer" $ testParse "T * ptr;" (DerivedPointer [] (BasicType [] "T"), "ptr")
      it "parses pointer to pointer" $ testParse "struct Node ** p;" (DerivedPointer [] (DerivedPointer [] (BasicType [] "struct Node")), "p")

    describe "9. Pointer Qualifiers (The Ultimate Test)" $ do
      it "parses pointer to const int" $ 
        testParse "const int * p;" (DerivedPointer [] (BasicType ["const"] "int"), "p")
        
      it "parses pointer to int const" $ 
        testParse "int const * p;" (DerivedPointer [] (BasicType ["const"] "int"), "p")
        
      it "parses const pointer to int" $ 
        testParse "int * const p;" (DerivedPointer ["const"] (BasicType [] "int"), "p")
        
      it "parses const pointer to const int" $ 
        testParse "const int * const p;" (DerivedPointer ["const"] (BasicType ["const"] "int"), "p")
        
      it "parses restrict pointer to struct" $ 
        testParse "struct Node * restrict p;" (DerivedPointer ["restrict"] (BasicType [] "struct Node"), "p")
        
      it "parses const restrict pointer to T" $ 
        testParse "T * const restrict p;" (DerivedPointer ["const", "restrict"] (BasicType [] "T"), "p")