module Juvix.Compiler.Concrete.Pretty.Ann where

import Juvix.Compiler.Concrete.Data

data Ann
  = AnnKind NameKind
  | AnnKeyword
  | AnnComment
  | AnnDelimiter
  | AnnLiteralString
  | AnnLiteralInteger
  | AnnUnkindedSym
  | AnnDef TopModulePath NameId
  | AnnRef TopModulePath NameId
