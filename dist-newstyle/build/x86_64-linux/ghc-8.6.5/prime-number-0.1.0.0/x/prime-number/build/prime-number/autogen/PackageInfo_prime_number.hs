{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_prime_number (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "prime_number"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "divider options"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://my-reference-link.blogspot.com/"
