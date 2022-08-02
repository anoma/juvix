module Juvix.Compiler.Backend.Haskell.Pretty.Ann where

import Juvix.Compiler.Concrete.Data.NameKind

data Ann
  = AnnKind NameKind
  | AnnKeyword
  | AnnLiteralString
  | AnnLiteralInteger
