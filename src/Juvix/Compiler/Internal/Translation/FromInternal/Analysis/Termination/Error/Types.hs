module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Error.Types where

import Juvix.Compiler.Internal.Language
import Juvix.Data.PPOutput
import Juvix.Prelude

newtype NoLexOrder = NoLexOrder
  { _noLexOrderFun :: NonEmpty Name
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
      names = _noLexOrderFun
      i = getLocSpan names

      single = case names of
        _ :| [] -> True
        _ -> False
      msg :: Doc Ann
      msg = do
        "The following"
          <+> function
          <+> fails
          <+> "the termination checker:"
            <> line
            <> itemize (fmap (code . pretty) names)
        where
          function :: Doc Ann
          function
            | single = "function"
            | otherwise = "functions"
          fails :: Doc Ann
          fails
            | single = "fails"
            | otherwise = "fail"
