module Juvix.Syntax.Abstract.Pretty.Ann where

import Juvix.Prelude
import Juvix.Syntax.Concrete.Scoped.Name qualified as S
import Juvix.Syntax.Concrete.Scoped.Pretty.Base qualified as S

data Ann
  = AnnKind S.NameKind
  | AnnKeyword
  | AnnImportant
  | AnnLiteralString
  | AnnLiteralInteger

fromScopedAnn :: S.Ann -> Maybe Ann
fromScopedAnn s = case s of
  S.AnnKind nk -> Just (AnnKind nk)
  S.AnnKeyword -> Nothing
  S.AnnDelimiter -> Nothing
  S.AnnUnkindedSym -> Nothing
  S.AnnDef {} -> Nothing
  S.AnnRef {} -> Nothing
  S.AnnLiteralString -> Just AnnLiteralInteger
  S.AnnLiteralInteger -> Just AnnLiteralString
