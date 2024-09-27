module Juvix.Extra.Paths.Base where

import Data.FileEmbed qualified as FE
import Juvix.Extra.Version
import Juvix.Prelude.Base
import Juvix.Prelude.Path
import Language.Haskell.TH.Syntax

assetsDirQ :: Q Exp
assetsDirQ = FE.makeRelativeToProject "assets" >>= FE.embedDir

cssDirQ :: Q Exp
cssDirQ = FE.makeRelativeToProject "assets/css" >>= FE.embedDir

juvixStyQ :: Q Exp
juvixStyQ = FE.embedFileRelative "include/latex/juvix.sty"

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

packageDescriptionDirContents :: Q Exp
packageDescriptionDirContents = FE.makeRelativeToProject (toFilePath packageDescriptionDir) >>= FE.embedDir

packageBaseDirContents :: Q Exp
packageBaseDirContents = FE.makeRelativeToProject (toFilePath packageBaseDir) >>= FE.embedDir

juvixYamlFile :: Path Rel File
juvixYamlFile = $(mkRelFile "juvix.yaml")

juvixLockfile :: Path Rel File
juvixLockfile = $(mkRelFile "juvix.lock.yaml")

packageDescriptionDir :: Path Rel Dir
packageDescriptionDir = $(mkRelDir "include/package")

packageBaseDir :: Path Rel Dir
packageBaseDir = $(mkRelDir "include/package-base")

v1PackageDescriptionFile :: Path Rel File
v1PackageDescriptionFile = $(mkRelFile "PackageDescription/V1.juvix")

v2PackageDescriptionFile :: Path Rel File
v2PackageDescriptionFile = $(mkRelFile "PackageDescription/V2.juvix")

basicPackageDescriptionFile :: Path Rel File
basicPackageDescriptionFile = $(mkRelFile "PackageDescription/Basic.juvix")

packageFilePath :: Path Rel File
packageFilePath = $(mkRelFile "Package.juvix")

relBuildDir :: Path Rel Dir
relBuildDir = $(mkRelDir ".juvix-build") <//> versionDir

relStdlibDir :: Path Rel Dir
relStdlibDir = $(mkRelDir "stdlib")

relDependenciesDir :: Path Rel Dir
relDependenciesDir = $(mkRelDir "deps")

rootBuildDir :: Path Abs Dir -> Path Abs Dir
rootBuildDir root = root <//> relBuildDir

juvixIncludeDir :: Path Abs Dir -> Path Abs Dir
juvixIncludeDir buildDir = buildDir <//> $(mkRelDir "include")

juvixStdlibDir :: Path Abs Dir -> Path Abs Dir
juvixStdlibDir buildDir = buildDir <//> relStdlibDir

preludePath :: Path Rel File
preludePath = $(mkRelFile "Stdlib/Prelude.juvix")

defaultStdlibPath :: Path Abs Dir -> Path Abs Dir
defaultStdlibPath buildDir = buildDir <//> $(mkRelDir "stdlib")
