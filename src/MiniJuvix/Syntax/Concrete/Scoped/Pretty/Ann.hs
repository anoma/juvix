module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ann where

import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import MiniJuvix.Syntax.Concrete.Language (TopModulePath)

data Ann
  = AnnKind S.NameKind
  | AnnKeyword
  | AnnDelimiter
  | AnnDef S.NameId
  | AnnRef TopModulePath S.NameId
