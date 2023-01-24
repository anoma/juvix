module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Error where

import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.Base
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver.PackageInfo
import Juvix.Compiler.Pipeline.Package
import Juvix.Data.CodeAnn
import Juvix.Prelude

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
            <> line
            <> "or in one of the dependencies:"
            <> line
            <> itemize (map pcode (_missingInfo ^.. packagePackage . packageDependencies . each . dependencyPath))
