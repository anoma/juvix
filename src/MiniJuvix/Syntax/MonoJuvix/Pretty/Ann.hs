module MiniJuvix.Syntax.MonoJuvix.Pretty.Ann where

import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind

data Ann
  = AnnKind NameKind
  | AnnKeyword
  | AnnLiteralString
  | AnnLiteralInteger
