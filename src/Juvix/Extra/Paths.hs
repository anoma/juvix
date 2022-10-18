module Juvix.Extra.Paths where

import Data.FileEmbed qualified as FE
import Juvix.Prelude
import Language.Haskell.TH.Syntax

assetsDir :: Q Exp
assetsDir = FE.makeRelativeToProject "assets" >>= FE.embedDir

stdlibDir :: Q Exp
stdlibDir = FE.makeRelativeToProject "juvix-stdlib" >>= FE.embedDir

juvixYamlFile :: FilePath
juvixYamlFile = "juvix.yaml"

juvixBuildDir :: FilePath
juvixBuildDir = ".juvix-build"

juvixStdlibDir :: FilePath
juvixStdlibDir = juvixBuildDir </> "stdlib"
