module Juvix.Syntax.Concrete.Scoped.Pretty.Ann where

import Juvix.Syntax.Concrete.Language (TopModulePath)
import Juvix.Syntax.Concrete.Scoped.Name qualified as S

data Ann
  = AnnKind S.NameKind
  | AnnKeyword
  | AnnComment
  | AnnDelimiter
  | AnnLiteralString
  | AnnLiteralInteger
  | AnnUnkindedSym
  | AnnDef TopModulePath S.NameId
  | AnnRef TopModulePath S.NameId
