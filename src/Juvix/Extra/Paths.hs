module Juvix.Extra.Paths where

import Data.FileEmbed qualified as FE
import Juvix.Prelude.Base
import Juvix.Prelude.Path
import Language.Haskell.TH.Syntax

assetsDir :: Q Exp
assetsDir = FE.makeRelativeToProject "assets" >>= FE.embedDir

stdlibDir :: Q Exp
stdlibDir = FE.makeRelativeToProject "juvix-stdlib" >>= FE.embedDir

juvixYamlFile :: FilePath
juvixYamlFile = "juvix.yaml"

juvixYamlFile' :: Path Rel File
juvixYamlFile' = $(mkRelFile "juvix.yaml")

juvixBuildDir :: FilePath
juvixBuildDir = ".juvix-build"

juvixStdlibDir :: FilePath
juvixStdlibDir = juvixBuildDir </> "stdlib"

preludePath :: FilePath
preludePath = "Stdlib" </> "Prelude.juvix"
