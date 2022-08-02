module Juvix.Compiler.Mono.Pretty.Ann where

import Juvix.Compiler.Concrete.Data.NameKind

data Ann
  = AnnKind NameKind
  | AnnKeyword
  | AnnLiteralString
  | AnnLiteralInteger
