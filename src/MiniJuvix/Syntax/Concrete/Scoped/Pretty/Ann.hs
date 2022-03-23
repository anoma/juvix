module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ann where

import MiniJuvix.Syntax.Concrete.Language (TopModulePath)
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S

data Ann
  = AnnKind S.NameKind
  | AnnKeyword
  | AnnDelimiter
  | AnnLiteralString
  | AnnLiteralInteger
  | AnnUnkindedSym
  | AnnDef TopModulePath S.NameId
  | AnnRef TopModulePath S.NameId
