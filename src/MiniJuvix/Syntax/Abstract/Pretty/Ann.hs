module MiniJuvix.Syntax.Abstract.Pretty.Ann where


import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base as S
import MiniJuvix.Prelude

data Ann =
  AnnKind S.NameKind
  | AnnKeyword
  | AnnImportant

fromScopedAnn :: S.Ann -> Maybe Ann
fromScopedAnn s = case s of
  S.AnnKind nk -> Just (AnnKind nk)
  S.AnnKeyword -> Nothing
  S.AnnDelimiter -> Nothing
  S.AnnUnkindedSym -> Nothing
  S.AnnNumber -> Nothing
  S.AnnDef {} -> Nothing
  S.AnnRef {} -> Nothing
