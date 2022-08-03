module Juvix.Compiler.Abstract.Pretty.Ann where

import Juvix.Compiler.Concrete.Data.NameKind
import Juvix.Compiler.Concrete.Pretty.Base qualified as S
import Juvix.Prelude

data Ann
  = AnnKind NameKind
  | AnnKeyword
  | AnnImportant
  | AnnLiteralString
  | AnnLiteralInteger

fromScopedAnn :: S.Ann -> Maybe Ann
fromScopedAnn s = case s of
  S.AnnKind nk -> Just (AnnKind nk)
  S.AnnKeyword -> Nothing
  S.AnnDelimiter -> Nothing
  S.AnnComment -> Nothing
  S.AnnUnkindedSym -> Nothing
  S.AnnDef {} -> Nothing
  S.AnnRef {} -> Nothing
  S.AnnLiteralString -> Just AnnLiteralInteger
  S.AnnLiteralInteger -> Just AnnLiteralString

instance HasNameKindAnn Ann where
  annNameKind = AnnKind
