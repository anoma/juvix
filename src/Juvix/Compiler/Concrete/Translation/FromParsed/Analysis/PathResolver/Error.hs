module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.PackageInfo
import Juvix.Compiler.Pipeline.Package
import Juvix.Data.CodeAnn
import Juvix.Data.Effect.Git
import Juvix.Prelude

data DependencyErrorGit = DependencyErrorGit
  { _dependencyErrorGitError :: GitError,
    _dependencyErrorGitCloneDir :: Path Abs Dir
  }

newtype DependencyErrorCause = GitDependencyError DependencyErrorGit

data DependencyError = DependencyError
  { _dependencyErrorPackageFile :: Path Abs File,
    _dependencyErrorCause :: DependencyErrorCause
  }

makeLenses ''DependencyError
makeLenses ''DependencyErrorGit

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

instance PrettyCodeAnn DependencyErrorGit where
  ppCodeAnn d = case d ^. dependencyErrorGitError of
    NotAClone ->
      prefix
        <> "The directory"
        <+> code (pretty (d ^. dependencyErrorGitCloneDir))
        <+> "is not a valid git clone."
          <> line
          <> "Try running"
        <+> code "juvix clean"
    NoSuchRef ref ->
      prefix
        <> "The git ref:"
        <+> code (pretty ref)
        <+> "does not exist in the clone:"
        <+> code (pretty (d ^. dependencyErrorGitCloneDir))
    where
      prefix :: Doc CodeAnn
      prefix = pretty @Text "Failed to obtain remote dependencies" <> line

data PathResolverError
  = ErrDependencyConflict DependencyConflict
  | ErrMissingModule MissingModule
  deriving stock (Show)

instance PrettyCodeAnn PathResolverError where
  ppCodeAnn = \case
    ErrDependencyConflict e -> ppCodeAnn e
    ErrMissingModule e -> ppCodeAnn e

data DependencyConflict = DependencyConflict
  { _conflictPackages :: NonEmpty PackageInfo,
    _conflictPath :: TopModulePath
  }
  deriving stock (Show)

instance PrettyCodeAnn DependencyConflict where
  ppCodeAnn DependencyConflict {..} =
    "The module name "
      <> code (pretty _conflictPath)
      <> " is ambiguous. It is defined in these packages:"
      <> line
      <> indent' (itemize (item <$> toList _conflictPackages))
    where
      item :: PackageInfo -> Doc CodeAnn
      item pkg = pcode (pkg ^. packagePackage . packageName) <+> "at" <+> pretty (pkg ^. packageRoot)

pcode :: (Pretty a) => a -> Doc CodeAnn
pcode = code . pretty

data MissingModule = MissingModule
  { _missingModule :: TopModulePath,
    _missingInfo :: PackageInfo
  }
  deriving stock (Show)

instance PrettyCodeAnn MissingModule where
  ppCodeAnn MissingModule {..} =
    "The module"
      <+> pcode _missingModule
      <+> "does not exist."
        <> line
        <> suggestion
    where
      suggestion :: Doc Ann
      suggestion =
        "It should be in"
          <+> pcode (_missingInfo ^. packageRoot <//> topModulePathToRelativePath' _missingModule)
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
          deps = _missingInfo ^. packagePackage . packageDependencies
