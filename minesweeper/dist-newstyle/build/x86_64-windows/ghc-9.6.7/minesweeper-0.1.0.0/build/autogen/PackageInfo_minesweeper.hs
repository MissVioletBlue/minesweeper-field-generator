{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_minesweeper (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "minesweeper"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A Minesweeper game implementation with TDD"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
