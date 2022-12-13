module Juvix.Extra.Paths.Base where

import Data.FileEmbed qualified as FE
import Juvix.Prelude
import Language.Haskell.TH.Syntax

assetsDirQ :: Q Exp
assetsDirQ = FE.makeRelativeToProject "assets" >>= FE.embedDir

projectFilePath :: Q Exp
projectFilePath = FE.makeRelativeToProject "." >>= lift

projectPath :: Q Exp
projectPath = FE.makeRelativeToProject "." >>= lift . absDir

stdlibDir :: Q Exp
stdlibDir = FE.makeRelativeToProject "juvix-stdlib" >>= FE.embedDir

juvixYamlFile :: Path Rel File
juvixYamlFile = $(mkRelFile "juvix.yaml")

juvixBuildDir :: Path Rel Dir
juvixBuildDir = $(mkRelDir ".juvix-build")

juvixIncludeDir :: Path Rel Dir
juvixIncludeDir = juvixBuildDir <//> $(mkRelDir "include")

juvixStdlibDir :: Path Rel Dir
juvixStdlibDir = juvixBuildDir <//> $(mkRelDir "stdlib")

preludePath :: Path Rel File
preludePath = $(mkRelFile "Stdlib/Prelude.juvix")

defaultStdlibPath :: Path Abs Dir -> Path Abs Dir
defaultStdlibPath root = root <//> juvixStdlibDir
