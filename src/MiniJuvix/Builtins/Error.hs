module MiniJuvix.Builtins.Error where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Concrete.Builtins
import MiniJuvix.Termination.Error.Pretty

data AlreadyDefined = AlreadyDefined
  { _alreadyDefinedBuiltin :: BuiltinPrim,
    _alreadyDefinedLoc :: Interval
  }

makeLenses ''AlreadyDefined

hh :: Doc Eann -> Doc Eann
hh = annotate Highlight

instance ToGenericError AlreadyDefined where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = e ^. alreadyDefinedLoc
      msg = "The builtin" <+> hh (pretty (e ^. alreadyDefinedBuiltin)) <+> "has already been defined"

data NotDefined = NotDefined
  { _notDefinedBuiltin :: BuiltinPrim,
    _notDefinedLoc :: Interval
  }

makeLenses ''NotDefined

instance ToGenericError NotDefined where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = e ^. notDefinedLoc
      msg = "The builtin" <+> hh (pretty (e ^. notDefinedBuiltin)) <+> "has not been defined"
