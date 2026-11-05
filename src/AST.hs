{-# LANGUAGE OverloadedStrings #-}
module AST where

import Prettyprinter

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

instance Pretty Declaration where
    pretty (GenericBlock typeVars structs) =
        "GenericBlock" <+> pretty (show typeVars) <+> "{" <> line <>
        indent 4 (vsep (map pretty structs)) <> line <> "}" <> line

    pretty (TypedefStruct identifier statements) =
        "TypedefStruct" <+> pretty (show identifier) <+> "{" <> line <>
        indent 4 (vsep (map (pretty . show) statements)) <> line <> "}"

    pretty (RawC code) =
        "RawC" <+> "{" <> line <>
        indent 4 (pretty code) <> line <> "}"

type Program = [Declaration]

-- instance Pretty CType where
--     pretty CInt = "int"
--     pretty (CPointer t) = pretty t <> "*"

-- prettyMember :: (CType, String) -> Doc ann
-- prettyMember (cType, ident) = pretty cType <+> pretty ident <> ";"

-- instance Pretty Declaration where

--     pretty (GenericBlock typeVars structs) = 
--         "/* GenericBlock for type vars:" <+> pretty typeVars <+> "*/" <> line <> 
--         vsep (map pretty structs) <> line 

--     pretty (TypedefStruct identifier statements) =
--         "typedef struct" <+> pretty identifier <+> "{" <> line <>
--         indent 4 (vsep (map prettyMember statements)) <> line <>
--         "}" <+> pretty identifier <> ";"

--     pretty (RawC code) = pretty code