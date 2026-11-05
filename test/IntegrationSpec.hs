module IntegrationSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import AST
import Parser.ParseProgram (pProgram)

spec :: Spec
spec = describe "Full File Integration" $ do
    it "successfully parses a generic struct file" $ do
        fileContent <- readFile "test/fixtures/simple_struct.gc"

        parse pProgram "simple_strcut.gc" `shouldSucceedOn` fileContent