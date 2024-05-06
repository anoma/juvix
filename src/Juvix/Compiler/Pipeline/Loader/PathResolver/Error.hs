module Juvix.Compiler.Pipeline.Loader.PathResolver.Error where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Pipeline.Loader.PathResolver.PackageInfo
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Data.CodeAnn
import Juvix.Data.Effect.Git
import Juvix.Prelude

data DependencyErrorGit = DependencyErrorGit
  { _dependencyErrorGitError :: GitError,
    _dependencyErrorGitCloneDir :: Path Abs Dir
  }

data MissingLockfileDependency = MissingLockfileDependency
  { _missingLockfileDependencyPath :: Path Abs File,
    _missingLockfileDependencyDependency :: Dependency
  }

data DependencyErrorCause
  = GitDependencyError DependencyErrorGit
  | MissingLockfileDependencyError MissingLockfileDependency

data DependencyError = DependencyError
  { _dependencyErrorPackageFile :: Path Abs File,
    _dependencyErrorCause :: DependencyErrorCause
  }

makeLenses ''DependencyError
makeLenses ''DependencyErrorGit
makeLenses ''MissingLockfileDependency

instance ToGenericError DependencyError where
  genericError e = do
    let msg = ppCodeAnn (e ^. dependencyErrorCause)
    return
      ( GenericError
          { _genericErrorMessage = mkAnsiText msg,
            _genericErrorLoc = i,
            _genericErrorIntervals = [i]
          }
      )
    where
      i = getLoc e

instance HasLoc DependencyError where
  getLoc e = singletonInterval (mkInitialLoc (e ^. dependencyErrorPackageFile))

instance PrettyCodeAnn DependencyErrorCause where
  ppCodeAnn = \case
    GitDependencyError e -> ppCodeAnn e
    MissingLockfileDependencyError e -> ppCodeAnn e

instance PrettyCodeAnn DependencyErrorGit where
  ppCodeAnn d = case d ^. dependencyErrorGitError of
    NotAClone ->
      prefix
        <> "The directory"
        <+> code (pretty (d ^. dependencyErrorGitCloneDir))
        <+> "is not a valid git clone."
          <> line
          <> "Try running"
        <+> code "juvix clean --global"
    NoSuchRef ref ->
      prefix
        <> "The git ref:"
        <+> code (pretty ref)
        <+> "does not exist in the clone:"
        <+> code (pretty (d ^. dependencyErrorGitCloneDir))
    where
      prefix :: Doc CodeAnn
      prefix = pretty @Text "Failed to obtain remote dependencies" <> line

instance PrettyCodeAnn MissingLockfileDependency where
  ppCodeAnn e =
    "The dependency"
      <+> code dependencyId
      <+> "is declared in the package's juvix.yaml but is not declared in the lockfile:"
      <+> lockfilePath
        <> line
        <> "Try running the following command:"
        <> line
        <> code "juvix dependencies update"
    where
      lockfilePath :: Doc CodeAnn
      lockfilePath = pretty (e ^. missingLockfileDependencyPath)
      dependencyId :: Doc CodeAnn
      dependencyId = case e ^. missingLockfileDependencyDependency of
        DependencyGit g -> pretty @Text (g ^. gitDependencyName)
        DependencyPath p -> pretty @Text (pack (p ^. pathDependencyPath . prepath))

data PathResolverError
  = ErrDependencyConflict DependencyConflict
  | ErrMissingModule MissingModule
  | ErrPackageInvalidImport PackageInvalidImport
  deriving stock (Show)

instance ToGenericError PathResolverError where
  genericError e =
    return $
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText $ ppCodeAnn e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc e

instance HasLoc PathResolverError where
  getLoc = \case
    ErrDependencyConflict DependencyConflict {..} ->
      getLoc _conflictPath
    ErrMissingModule MissingModule {..} ->
      getLoc _missingModule
    ErrPackageInvalidImport PackageInvalidImport {..} ->
      getLoc _packageInvalidImport

instance PrettyCodeAnn PathResolverError where
  ppCodeAnn = \case
    ErrDependencyConflict e -> ppCodeAnn e
    ErrMissingModule e -> ppCodeAnn e
    ErrPackageInvalidImport e -> ppCodeAnn e

data DependencyConflict = DependencyConflict
  { _conflictPackages :: NonEmpty PackageInfo,
    _conflictPath :: ImportScan
  }
  deriving stock (Show)

instance PrettyCodeAnn DependencyConflict where
  ppCodeAnn DependencyConflict {..} =
    "The module name "
      <> importScanPrettyName _conflictPath
      <> " is ambiguous. It is defined in these packages:"
      <> line
      <> indent' (itemize (item <$> toList _conflictPackages))
    where
      item :: PackageInfo -> Doc CodeAnn
      item pkg = pcode (pkg ^. packagePackage . packageLikeName) <+> "at" <+> pretty (pkg ^. packageRoot)

pcode :: (Pretty a) => a -> Doc CodeAnn
pcode = code . pretty

data MissingModule = MissingModule
  { _missingModule :: ImportScan,
    _missingInfo :: PackageInfo
  }
  deriving stock (Show)

instance PrettyCodeAnn MissingModule where
  ppCodeAnn MissingModule {..} =
    "The module"
      <+> importScanPrettyName _missingModule
      <+> "does not exist."
        <> line
        <> suggestion
    where
      suggestion :: Doc Ann
      suggestion =
        "It should be in"
          <+> pcode (_missingInfo ^. packageRoot <//> addFileExt FileExtJuvix (importScanToRelPath _missingModule))
            <> dependenciesSuggestion

      dependenciesSuggestion :: Doc Ann
      dependenciesSuggestion
        | null deps = mempty
        | otherwise =
            line
              <> "or in one of the dependencies:"
              <> line
              <> itemize (map pcode deps)
        where
          deps :: [Dependency]
          deps = _missingInfo ^. packagePackage . packageLikeDependencies

newtype PackageInvalidImport = PackageInvalidImport
  {_packageInvalidImport :: ImportScan}
  deriving stock (Show)

instance PrettyCodeAnn PackageInvalidImport where
  ppCodeAnn PackageInvalidImport {..} =
    "The module"
      <+> pcode _packageInvalidImport
      <+> "cannot be imported by the Package file."
        <> line
        <> "Package files may only import modules from the Juvix standard library, Juvix.Builtin modules, or from the PackageDescription module."
