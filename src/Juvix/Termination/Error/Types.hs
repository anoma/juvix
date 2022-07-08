module Juvix.Termination.Error.Types where

import Juvix.Prelude
import Juvix.Prelude.Pretty
import Juvix.Syntax.Abstract.Language
import Juvix.Termination.Error.Pretty

newtype NoLexOrder = NoLexOrder
  { _noLexOrderFun :: Name
  }
  deriving stock (Show)

makeLenses 'NoLexOrder

instance ToGenericError NoLexOrder where
  genericError NoLexOrder {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      name = _noLexOrderFun
      i = getLoc name

      msg :: Doc Eann
      msg =
        "The function"
          <+> highlight (pretty name)
          <+> "fails the termination checker."
