module Juvix.Compiler.Core.Pretty.Ann where

import Juvix.Compiler.Concrete.Data.NameKind

data Ann
  = AnnKind NameKind
  | AnnKeyword
  | AnnLiteralString
  | AnnLiteralInteger

instance HasNameKindAnn Ann where
  annNameKind = AnnKind
