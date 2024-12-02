module Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
  ( module Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo,
    module Juvix.Compiler.Concrete.Translation.ImportScanner.Base,
  )
where

import Data.HashSet qualified as HashSet
import Data.Versions
import Juvix.Compiler.Concrete.Translation.ImportScanner.Base
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

data PackageLike
  = PackageReal Package
  | PackageStdlibInGlobalPackage
  | PackageBase
  | PackageType
  | PackageDotJuvix
  deriving stock (Show)

data PackageInfo = PackageInfo
  { _packageRoot :: Path Abs Dir,
    -- | Files relative to the root of the package. Does include all .juvix and
    -- .juvix.md files. Note that it should not contain Package.juvix.
    _packageJuvixRelativeFiles :: HashSet (Path Rel File),
    _packageAvailableRoots :: HashSet (Path Abs Dir),
    _packageInfoPackageId :: PackageId,
    _packagePackage :: PackageLike
  }
  deriving stock (Show)

makeLenses ''PackageInfo
makePrisms ''PackageLike

packageInfoFiles :: PackageInfo -> [Path Abs File]
packageInfoFiles k = [k ^. packageRoot <//> f | f <- (toList (k ^. packageJuvixRelativeFiles))]

-- | Does *not* include Package.juvix
packageJuvixFiles :: SimpleGetter PackageInfo (HashSet (Path Rel File))
packageJuvixFiles =
  to $ keepJuvixFiles . (^. packageJuvixRelativeFiles)

keepJuvixFiles :: HashSet (Path Rel File) -> HashSet (Path Rel File)
keepJuvixFiles = HashSet.filter isJuvixOrJuvixMdFile

packageLikeName :: SimpleGetter PackageLike Text
packageLikeName = to $ \case
  PackageReal r -> r ^. packageName
  PackageStdlibInGlobalPackage {} -> "global-stdlib"
  PackageBase -> Str.packageBase
  PackageType -> "package-type"
  PackageDotJuvix -> "package-dot-juvix"

packageInfoNameAndVersion ::
  PackageInfo ->
  Doc CodeAnn
packageInfoNameAndVersion n =
  let pid = n ^. packageInfoPackageId
   in annotate AnnImportant (pretty (pid ^. packageIdName))
        <+> pretty (prettySemVer (pid ^. packageIdVersion))

packageLikeDependencies :: SimpleGetter PackageLike [Dependency]
packageLikeDependencies = to $ \case
  PackageReal r -> r ^. packageDependencies
  PackageStdlibInGlobalPackage {} -> impossible
  PackageBase -> []
  PackageType -> []
  PackageDotJuvix -> []

packageLikeFile :: PackageLike -> Path Abs File
packageLikeFile = \case
  PackageReal r -> r ^. packageFile
  PackageStdlibInGlobalPackage {} -> impossible
  PackageBase -> impossible
  PackageType -> impossible
  PackageDotJuvix -> impossible
