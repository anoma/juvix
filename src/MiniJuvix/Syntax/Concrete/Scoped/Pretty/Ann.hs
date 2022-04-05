module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ann where

import MiniJuvix.Syntax.Concrete.Language (TopModulePath)
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S

data Ann
  = AnnKind S.NameKind
  | AnnKeyword
  | AnnDelimiter
  | AnnLiteralString
  | AnnLiteralInteger
  | AnnUnkindedSym
  | AnnDef TopModulePath S.NameId
  | AnnRef TopModulePath S.NameId
