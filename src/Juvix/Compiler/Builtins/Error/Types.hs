module Juvix.Compiler.Builtins.Error.Types where

import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Data.PPOutput
import Juvix.Prelude

data AlreadyDefined = AlreadyDefined
  { _alreadyDefinedBuiltin :: BuiltinPrim,
    _alreadyDefinedLoc :: Interval
  }

makeLenses ''AlreadyDefined

instance ToGenericError AlreadyDefined where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = ppOutput msg,
          _genericErrorIntervals = [i]
        }
    where
      i = e ^. alreadyDefinedLoc
      msg = "The builtin" <+> code (pretty (e ^. alreadyDefinedBuiltin)) <+> "has already been defined"

data NotDefined = NotDefined
  { _notDefinedBuiltin :: BuiltinPrim,
    _notDefinedLoc :: Interval
  }

makeLenses ''NotDefined

instance ToGenericError NotDefined where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = ppOutput msg,
          _genericErrorIntervals = [i]
        }
    where
      i = e ^. notDefinedLoc
      msg = "The builtin" <+> code (pretty (e ^. notDefinedBuiltin)) <+> "has not been defined"

-- | Generic error message related to builtins
data BuiltinsErrorMessage = BuiltinsErrorMessage
  { _builtinsErrorMessage :: AnsiText,
    _builtinsErrorMessageLoc :: Interval
  }

instance ToGenericError BuiltinsErrorMessage where
  genericError BuiltinsErrorMessage {..} =
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = _builtinsErrorMessage,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = _builtinsErrorMessageLoc
