module Juvix.Syntax.MonoJuvix.Pretty.Ann where

import Juvix.Syntax.Concrete.Scoped.Name.NameKind

data Ann
  = AnnKind NameKind
  | AnnKeyword
  | AnnLiteralString
  | AnnLiteralInteger
