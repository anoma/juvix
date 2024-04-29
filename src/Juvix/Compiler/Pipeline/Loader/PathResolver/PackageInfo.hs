module Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
  ( module Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo,
    module Juvix.Compiler.Concrete.Translation.ImportScanner.Base,
  )
where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Concrete.Translation.ImportScanner.Base
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Extra.Paths.Base
import Juvix.Prelude

data PackageLike
  = PackageReal Package
  | PackageGlobalStdlib
  | PackageBase
  | PackageType
  | PackageDotJuvix
  deriving stock (Show)

data PackageInfo = PackageInfo
  { _packageRoot :: Path Abs Dir,
    -- | Files relative to the root of the package.
    _packageRelativeFilesTmp :: HashSet (Path Rel File),
    -- | All files in _packageRelativeFiles are keys in this map, even if the
    -- mapped HashSet is empty.
    _packageImports :: HashMap (Path Rel File) (HashSet ImportScan),
    _packageAvailableRoots :: HashSet (Path Abs Dir),
    _packagePackage :: PackageLike
  }
  deriving stock (Show)

makeLenses ''PackageInfo

packageFiles :: PackageInfo -> [Path Abs File]
packageFiles k = [k ^. packageRoot <//> f | f <- toList (k ^. packageRelativeFilesTmp)]

-- | Does *not* include Package.juvix
packageJuvixFiles :: SimpleGetter PackageInfo (HashSet (Path Rel File))
packageJuvixFiles =
  to $ keepJuvixFiles . (^. packageRelativeFilesTmp)

keepJuvixFiles :: HashSet (Path Rel File) -> HashSet (Path Rel File)
keepJuvixFiles = HashSet.filter ((/= packageFilePath) .&&. isJuvixOrJuvixMdFile)

packageLikeName :: SimpleGetter PackageLike Text
packageLikeName = to $ \case
  PackageReal r -> r ^. packageName
  PackageGlobalStdlib -> "global-stdlib"
  PackageBase -> "package-base"
  PackageType -> "package-type"
  PackageDotJuvix -> "package-dot-juvix"

packageLikeDependencies :: SimpleGetter PackageLike [Dependency]
packageLikeDependencies = to $ \case
  PackageReal r -> r ^. packageDependencies
  PackageGlobalStdlib -> []
  PackageBase -> []
  PackageType -> []
  PackageDotJuvix -> []

packageLikeFile :: SimpleGetter PackageLike (Path Abs File)
packageLikeFile = to $ \case
  PackageReal r -> r ^. packageFile
  PackageGlobalStdlib -> impossible
  PackageBase -> impossible
  PackageType -> impossible
  PackageDotJuvix -> impossible
