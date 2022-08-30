module Juvix.Data.Effect.Files.Error where

import Juvix.Data.Error
import Juvix.Data.Loc
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty

data FilesErrorCause = StdlibConflict
  deriving stock (Show)

data FilesError = FilesError
  { _filesErrorPath :: FilePath,
    _filesErrorCause :: FilesErrorCause
  }
  deriving stock (Show)

instance ToGenericError FilesError where
  genericError FilesError {..} =
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = AnsiText (pretty @_ @AnsiStyle msg),
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i =
        Interval
          { _intervalFile = _filesErrorPath,
            _intervalStart = noFileLoc,
            _intervalEnd = noFileLoc
          }
      msg :: Text
      msg = case _filesErrorCause of
        StdlibConflict -> "The module defined in " <> pack _filesErrorPath <> " conflicts with a module defined in the standard library."

noFileLoc :: FileLoc
noFileLoc =
  FileLoc
    { _locLine = mempty,
      _locCol = mempty,
      _locOffset = mempty
    }

makeLenses ''FilesError
