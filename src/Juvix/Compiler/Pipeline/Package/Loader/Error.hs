module Juvix.Compiler.Pipeline.Package.Loader.Error where

import Juvix.Data.CodeAnn
import Juvix.Prelude

data PackageLoaderError = PackageLoaderError
  { _packageLoaderErrorPath :: Path Abs File,
    _packageLoaderErrorCause :: PackageLoaderErrorCause
  }

data PackageLoaderErrorCause
  = ErrPackageYamlParseError PackageYamlParseError
  | ErrLockfileYamlParseError LockfileYamlParseError
  | ErrVersionParseError VersionParseError
  | ErrDuplicateDependencyError DuplicateDependencyError

newtype PackageYamlParseError = PackageYamlParseError
  { _packageYamlParseErrorError :: Text
  }

newtype LockfileYamlParseError = LockfileYamlParseError
  { _lockfileYamlParseErrorError :: Text
  }

newtype VersionParseError = VersionParseError
  { _versionParseErrorError :: Text
  }

newtype DuplicateDependencyError = DuplicateDependencyError
  { _duplicateDependencyErrorName :: Text
  }

makeLenses ''PackageLoaderError
makeLenses ''PackageYamlParseError
makeLenses ''LockfileYamlParseError
makeLenses ''VersionParseError
makeLenses ''DuplicateDependencyError

instance ToGenericError PackageLoaderError where
  genericError e = do
    let msg = mkAnsiText (ppCodeAnn e)
    return
      GenericError
        { _genericErrorMessage = msg,
          _genericErrorLoc = i,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc e

instance PrettyCodeAnn PackageLoaderError where
  ppCodeAnn e = ppCodeAnn (e ^. packageLoaderErrorCause)

instance PrettyCodeAnn PackageLoaderErrorCause where
  ppCodeAnn = \case
    ErrPackageYamlParseError e -> ppCodeAnn e
    ErrLockfileYamlParseError e -> ppCodeAnn e
    ErrVersionParseError e -> ppCodeAnn e
    ErrDuplicateDependencyError e -> ppCodeAnn e

instance PrettyCodeAnn PackageYamlParseError where
  ppCodeAnn e =
    "The package file is invalid"
      <> line
      <+> pretty (e ^. packageYamlParseErrorError)

instance PrettyCodeAnn LockfileYamlParseError where
  ppCodeAnn e =
    "The lock file is invalid"
      <> line
      <+> pretty (e ^. lockfileYamlParseErrorError)

instance PrettyCodeAnn VersionParseError where
  ppCodeAnn e =
    "The package version is invalid"
      <> line
      <+> pretty (e ^. versionParseErrorError)

instance PrettyCodeAnn DuplicateDependencyError where
  ppCodeAnn e =
    "Juvix package file contains the duplicate dependency name:"
      <+> pretty (e ^. duplicateDependencyErrorName)

instance HasLoc PackageLoaderError where
  getLoc e = singletonInterval (mkInitialLoc (e ^. packageLoaderErrorPath))
