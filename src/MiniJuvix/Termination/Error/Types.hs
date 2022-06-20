module MiniJuvix.Termination.Error.Types where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Abstract.Language
import MiniJuvix.Termination.Error.Pretty

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
        "The function" <+> highlight (pretty name)
          <+> "fails the termination checker."
