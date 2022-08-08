module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Types where

import Juvix.Compiler.Abstract.Language
import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty
import Juvix.Prelude
import Juvix.Prelude.Pretty

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

      msg :: Doc Ann
      msg =
        "The function"
          <+> highlight (pretty name)
          <+> "fails the termination checker."
