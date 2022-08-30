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
  genericError e = ask >>= generr
    where
      generr opts =
        return GenericError
          { _genericErrorLoc = i,
            _genericErrorMessage = ppOutput msg,
            _genericErrorIntervals = [i]
          }
        where
          i = getLoc e
          opts' = fromGenericOptions opts
          msg = case e ^. coreErrorNode of
            Just node -> pretty (e ^. coreErrorMsg) <> colon <> space <> pretty (ppTrace' opts' node)
            Nothing -> pretty (e ^. coreErrorMsg)

instance Pretty CoreError where
  pretty (CoreError {..}) = case _coreErrorNode of
    Just node -> pretty _coreErrorMsg <> colon <> space <> pretty (ppTrace node)
    Nothing -> pretty _coreErrorMsg

instance HasLoc CoreError where
  getLoc (CoreError {..}) = _coreErrorLoc
