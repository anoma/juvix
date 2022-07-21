module Juvix.Syntax.MicroJuvix.Pretty.Ann where

import Juvix.Syntax.Concrete.Scoped.Name.NameKind

data Ann
  = AnnKind NameKind
  | AnnKeyword
  | AnnLiteralString
  | AnnLiteralInteger

instance HasNameKindAnn Ann where
  annNameKind = AnnKind
