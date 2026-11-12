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

-- | Used for function bodies
type TemplateDeclaration = [BodyChunk]

data BodyChunk
    = RawBodyText String
    | TemplateCall Identifier [FullType]
    deriving (Show, Eq)

type Program = [TemplateDefinition]

-- | Alias for String. Used for primitive (char, int, float, double etc.),
-- composite (aggregate) and user-defined types (structs, unions, enums and typedefs).
type BaseType = Identifier 

-- | Alias for String. Used for type qualifier (const, unsigned, long, volatile etc.).
type QualifierType = Identifier 

-- | Full type declaration.
data FullType
    = BasicType [QualifierType] BaseType
    -- ^ Represent basic, non-pointer or array variable declarations.
    -- Stores the list of qualifiers (long, short, signed etc.) and the base type (char, int, float, structs, unions etc.).

    | DerivedPointer [QualifierType] FullType
    -- ^ Stores the pointer qualifiers (const, volatile, restrict, _Atomic etc.) and the type (basic or derived) it points to.

    | DerivedArray FullType (Maybe String)
    -- ^ Stores the type (basic or derived) and the array size if specified.

    | DerivedFuncPointer FullType [FullType]
    -- ^ Stores the return type (basic or derived) and a list of types for the arguments.

    | TemplateType [QualifierType] Identifier [FullType]

    deriving (Show, Eq)

-- | Represents the core structural elements (AST nodes) of a GenC file.
-- This type captures both custom template boundaries and standard C constructs,
-- allowing the transpiler to isolate generic code from standard C code.
data TemplateDefinition
                 = GenericBlock [Identifier] [TemplateDefinition]
                   -- ^ A generic template block (e.g., @generic T { ... }@). 
                   -- Stores the list of generic type parameters and the block's inner body.

                 | TypedefStruct Identifier [Statement]
                   -- ^ A C struct declaration (e.g., @typedef struct { ... } Name;@).
                   -- Stores the name of the struct and a list of its member fields.

                 | Function FullType Identifier [Statement] TemplateDeclaration
                   -- ^ A C function definition. 
                   -- Stores the return type, function name, arguments list, and the raw function body.
                 | RawC [BodyChunk]
                   -- ^ Pass-through C code.
                   -- Unparseable lines, or standard C code that doesn't need template substitution.

                 deriving (Show, Eq)

instance Pretty BodyChunk where
    pretty (RawBodyText text) = 
        "RawBodyText" <> line <> 
        indent 4 (vsep (map pretty  (lines text)))
        
    pretty (TemplateCall funcName args) = 
        "TemplateCall" <+> pretty (show funcName) <+> pretty (show args)

instance Pretty TemplateDefinition where
    pretty :: TemplateDefinition -> Doc ann
    pretty (GenericBlock typeVars structs) =
        "GenericBlock" <+> pretty (show typeVars) <+> "[" <> line <>
        indent 4 (vsep (map pretty structs)) <> line <> "]" <> line

    pretty (TypedefStruct identifier statements) =
        "TypedefStruct" <+> pretty (show identifier) <+> "[" <> line <>
        indent 4 (vsep (map (pretty . show) statements)) <> line <> "]"

    pretty (RawC chunks) =
        "RawC" <+> "[" <> line <>
        indent 4 (vsep (map pretty chunks)) <> line <> "]"

    pretty (Function retType name args body) =
        vsep [ "Function" <+> pretty (show retType) <+> pretty (show name) <+> pretty (show args) <+> "["
             , indent 4 (vsep (map pretty body))
             , "]"
             ]