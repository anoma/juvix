module Juvix.Extra.Paths.Base where

import Data.FileEmbed qualified as FE
import Juvix.Prelude
import Language.Haskell.TH.Syntax

assetsDirQ :: Q Exp
assetsDirQ = FE.makeRelativeToProject "assets" >>= FE.embedDir

cssDirQ :: Q Exp
cssDirQ = FE.makeRelativeToProject "assets/css" >>= FE.embedDir

jsDirQ :: Q Exp
jsDirQ = FE.makeRelativeToProject "assets/js" >>= FE.embedDir

imagesDirQ :: Q Exp
imagesDirQ = FE.makeRelativeToProject "assets/images" >>= FE.embedDir


projectFilePath :: Q Exp
projectFilePath = FE.makeRelativeToProject "." >>= lift

projectPath :: Q Exp
projectPath = FE.makeRelativeToProject "." >>= lift . absDir

stdlibDir :: Q Exp
stdlibDir = FE.makeRelativeToProject "juvix-stdlib" >>= FE.embedDir

juvixYamlFile :: Path Rel File
juvixYamlFile = $(mkRelFile "juvix.yaml")

relBuildDir :: Path Rel Dir
relBuildDir = $(mkRelDir ".juvix-build")

rootBuildDir :: Path Abs Dir -> Path Abs Dir
rootBuildDir root = root <//> relBuildDir

juvixIncludeDir :: Path Abs Dir -> Path Abs Dir
juvixIncludeDir buildDir = buildDir <//> $(mkRelDir "include")

juvixStdlibDir :: Path Abs Dir -> Path Abs Dir
juvixStdlibDir buildDir = buildDir <//> $(mkRelDir "stdlib")

preludePath :: Path Rel File
preludePath = $(mkRelFile "Stdlib/Prelude.juvix")

defaultStdlibPath :: Path Abs Dir -> Path Abs Dir
defaultStdlibPath buildDir = buildDir <//> $(mkRelDir "stdlib")
