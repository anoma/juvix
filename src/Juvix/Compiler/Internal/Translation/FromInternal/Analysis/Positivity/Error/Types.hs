module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Error.Types where

import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty (fromGenericOptions)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error.Pretty
import Juvix.Data.PPOutput
import Juvix.Prelude

data TypeInNegativePosition = TypeInNegativePosition
  { _typeInNegativePositionType :: Name,
    _typeInNegativePositionConstructor :: Name,
    _typeInNegativePositionArgument :: Expression
  }

makeLenses ''TypeInNegativePosition

instance ToGenericError TypeInNegativePosition where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = j,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i, j]
            }
        where
          opts' = fromGenericOptions opts
          ty = e ^. typeInNegativePositionType
          ctor = e ^. typeInNegativePositionConstructor
          arg = e ^. typeInNegativePositionArgument
          i = getLoc ty
          j = getLoc arg
          msg =
            "The type"
              <+> ppCode opts' ty
              <+> "is not strictly positive."
              <> line
              <> "It appears at a negative position in one of the type arguments of the constructor"
              <+> ppCode opts' ctor
              <> "."

data TypeAsArgumentOfBoundVar = TypeAsArgumentOfBoundVar
  { _typeAsArgumentOfBoundVarType :: Name,
    _typeAsArgumentOfBoundVarConstructor :: Name,
    _typeAsArgumentOfBoundVarReference :: Expression
  }

makeLenses ''TypeAsArgumentOfBoundVar

instance ToGenericError TypeAsArgumentOfBoundVar where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = j,
              _genericErrorMessage = ppOutput msg,
              _genericErrorIntervals = [i, j]
            }
        where
          opts' = fromGenericOptions opts
          ty = e ^. typeAsArgumentOfBoundVarType
          ctor = e ^. typeAsArgumentOfBoundVarConstructor
          var = e ^. typeAsArgumentOfBoundVarReference
          i = getLoc ty
          j = getLoc var
          msg =
            "The type"
              <+> ppCode opts' ty
              <+> "is not strictly positive."
              <> line
              <> "It appears as an argument of the bound variable"
              <+> ppCode opts' var
              <+> "in one of the type arguments of the constructor"
              <+> ppCode opts' ctor
              <> "."
