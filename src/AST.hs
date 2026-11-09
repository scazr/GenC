{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module AST where

import Prettyprinter

import Data.Void
import Text.Megaparsec


type Parser = Parsec Void String
type Identifier = String
-- | Contains variable type and name.
type Statement = (FullType, Identifier)
type TemplateDeclaration = String
type Program = [TemplateDefinition]

-- | Alias for String. Used for primitive (char, int, float, double etc.),
-- composite (aggregate) and user-defined types (structs, unions, enums and typedefs).
type BaseType = Identifier 

-- | Alias for String. Used for type qualifier (const, unsigned, long, volatile etc.).
type QualifierType = Identifier 

-- | Alias for ([String], String). Represent the base type + all its qualifiers.
-- type BasicType = ([QualifierType], BaseType)

-- | Full type declaration. Basic types represent normal variable declarations. \n
-- - Derived types are composed of Basic type + additional info.
-- - DerivedPointer has [QualifierType] for it's own pointers qualifiers. \n
-- - DerivedArray has a Maybe String field in case of size declaration. \n
-- - DerivedFuncPointer has one FullType field for it's return type and [FullType] for it's return types.
data FullType = BasicType [QualifierType] BaseType
              | DerivedPointer [QualifierType] FullType
              | DerivedArray FullType (Maybe String)
              | DerivedFuncPointer FullType [FullType]
              deriving (Show, Eq)

data CType = CInt
           | CFloat
           | CCustom Identifier
           | CPointer CType
           deriving (Show, Eq)

data TemplateDefinition = GenericBlock [Identifier] [TemplateDefinition]
                 | TypedefStruct Identifier [Statement]
                 | Function FullType Identifier [Statement] TemplateDeclaration
                 | RawC String
                 deriving (Show, Eq)


instance Pretty TemplateDefinition where
    pretty :: TemplateDefinition -> Doc ann
    pretty (GenericBlock typeVars structs) =
        "GenericBlock" <+> pretty (show typeVars) <+> "[" <> line <>
        indent 4 (vsep (map pretty structs)) <> line <> "]" <> line

    pretty (TypedefStruct identifier statements) =
        "TypedefStruct" <+> pretty (show identifier) <+> "[" <> line <>
        indent 4 (vsep (map (pretty . show) statements)) <> line <> "]"

    pretty (RawC code) =
        "RawC" <+> "[" <> line <>
        indent 4 (pretty code) <> line <> "]"

    pretty (Function retType name args body) =
        vsep [ "Function" <+> pretty (show retType) <+> pretty (show name) <+> pretty (show args) <+> "["
            , indent 4 (vsep (map pretty (lines body)))
            , "]"
            ]
