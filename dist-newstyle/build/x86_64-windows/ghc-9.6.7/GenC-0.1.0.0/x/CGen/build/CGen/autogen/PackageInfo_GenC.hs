{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_GenC (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "GenC"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "C transpiler to support generics."
copyright :: String
copyright = ""
homepage :: String
homepage = ""
