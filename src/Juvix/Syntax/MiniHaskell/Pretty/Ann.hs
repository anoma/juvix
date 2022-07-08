module Juvix.Syntax.MiniHaskell.Pretty.Ann where

import Juvix.Syntax.Concrete.Scoped.Name.NameKind

data Ann
  = AnnKind NameKind
  | AnnKeyword
  | AnnLiteralString
  | AnnLiteralInteger
