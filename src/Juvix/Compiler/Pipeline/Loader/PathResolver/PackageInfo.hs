module Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
  ( module Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo,
    module Juvix.Compiler.Concrete.Translation.ImportScanner.Base,
    module Juvix.Compiler.Pipeline.Loader.PathResolver.GlobalVersions,
  )
where

import Data.HashSet qualified as HashSet
import Data.Versions
import Juvix.Compiler.Concrete.Translation.ImportScanner.Base
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Loader.PathResolver.GlobalVersions
import Juvix.Data.CodeAnn
import Juvix.Extra.Strings qualified as Str
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
    -- | Files relative to the root of the package. Does include all .juvix and
    -- .juvix.md files. Note that it should not contain Package.juvix.
    _packageJuvixRelativeFiles :: HashSet (Path Rel File),
    _packageAvailableRoots :: HashSet (Path Abs Dir),
    _packagePackage :: PackageLike
  }
  deriving stock (Show)

makeLenses ''PackageInfo
makePrisms ''PackageLike

packageFiles :: PackageInfo -> [Path Abs File]
packageFiles k = [k ^. packageRoot <//> f | f <- toList (k ^. packageJuvixRelativeFiles)]

-- | Does *not* include Package.juvix
packageJuvixFiles :: SimpleGetter PackageInfo (HashSet (Path Rel File))
packageJuvixFiles =
  to $ keepJuvixFiles . (^. packageJuvixRelativeFiles)

keepJuvixFiles :: HashSet (Path Rel File) -> HashSet (Path Rel File)
keepJuvixFiles = HashSet.filter isJuvixOrJuvixMdFile

packageLikePackageId :: (Members '[Reader GlobalVersions] r) => PackageLike -> Sem r PackageId
packageLikePackageId p = do
  ver <- packageLikeVersion p
  return
    PackageId
      { _packageIdName = p ^. packageLikeName,
        _packageIdVersion = ver
      }

packageLikeName :: SimpleGetter PackageLike Text
packageLikeName = to $ \case
  PackageReal r -> r ^. packageName
  PackageGlobalStdlib -> "global-stdlib"
  PackageBase -> Str.packageBase
  PackageType -> "package-type"
  PackageDotJuvix -> "package-dot-juvix"

packageLikeVersion :: (Members '[Reader GlobalVersions] r) => PackageLike -> Sem r SemVer
packageLikeVersion = \case
  PackageReal pkg -> return (pkg ^. packageVersion)
  PackageGlobalStdlib {} -> fromMaybe err <$> asks (^. globalVersionsStdlib)
  PackageBase {} -> return defaultVersion
  PackageType {} -> return defaultVersion
  PackageDotJuvix {} -> return defaultVersion
  where
    err :: a
    err = impossibleError "Asked the version of the global standard library but wasn't there"

packageLikeNameAndVersion ::
  (Members '[Reader GlobalVersions] r) =>
  PackageLike ->
  Sem r (Doc CodeAnn)
packageLikeNameAndVersion n = do
  v <- packageLikeVersion n
  return
    ( annotate AnnImportant (pretty (n ^. packageLikeName))
        <+> pretty (prettySemVer v)
    )

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
