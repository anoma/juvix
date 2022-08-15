module Juvix.Compiler.Core.Error where

import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Pretty

data CoreError = CoreError
  { _coreErrorMsg :: Text,
    _coreErrorNode :: Maybe Node,
    _coreErrorLoc :: Location
  }

makeLenses ''CoreError

instance ToGenericError CoreError where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = AnsiText $ pretty @_ @AnsiStyle e,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc e

instance Pretty CoreError where
  pretty (CoreError {..}) = case _coreErrorNode of
    Just node -> pretty _coreErrorMsg <> colon <> space <> pretty (ppTrace node)
    Nothing -> pretty _coreErrorMsg

instance HasLoc CoreError where
  getLoc (CoreError {..}) = _coreErrorLoc
