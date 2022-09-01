module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Types where

import Juvix.Compiler.Abstract.Language
import Juvix.Data.PPOutput
import Juvix.Prelude

newtype NoLexOrder = NoLexOrder
  { _noLexOrderFun :: Name
  }
  deriving stock (Show)

makeLenses 'NoLexOrder

instance ToGenericError NoLexOrder where
  genericError NoLexOrder {..} =
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = ppOutput msg,
          _genericErrorIntervals = [i]
        }
    where
      name = _noLexOrderFun
      i = getLoc name

      msg :: Doc Ann
      msg =
        "The function"
          <+> code (pretty name)
          <+> "fails the termination checker."
