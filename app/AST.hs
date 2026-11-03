module AST where

import Data.Void
import Text.Megaparsec


type Parser = Parsec Void String

type Identifier = String

data CType = CInt 
           | CFloat
           | CCustom Identifier
           | CPointer CType
           deriving (Show, Eq)

data Declaration = GenericBlock [Identifier] [Declaration]
                 | TypedefStruct Identifier [(CType, Identifier)]
                 | RawC String
                 deriving (Show, Eq)

type Program = [Declaration]