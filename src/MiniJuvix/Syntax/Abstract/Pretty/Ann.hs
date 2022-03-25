module MiniJuvix.Syntax.Abstract.Pretty.Ann where

import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base as S

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
