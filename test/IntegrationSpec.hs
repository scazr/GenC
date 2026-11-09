module IntegrationSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import AST
import Parser.ParseProgram (pProgram)

spec :: Spec
spec = describe "Full File Integration" $ do
    it "successfully parses a generic struct file" $ do
        fileContent <- readFile "test/fixtures/minimal_struct.gc"

        parse pProgram "minimal_struct.gc" `shouldSucceedOn` fileContent