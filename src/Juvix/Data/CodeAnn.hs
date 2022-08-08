module Juvix.Data.CodeAnn where

import Juvix.Compiler.Concrete.Data
import Juvix.Prelude
import Prettyprinter.Render.Terminal

type Ann = CodeAnn

data CodeAnn
  = AnnKind NameKind
  | AnnKeyword
  | AnnCode
  | AnnComment
  | AnnImportant
  | AnnDelimiter
  | AnnLiteralString
  | AnnLiteralInteger
  | AnnUnkindedSym
  | AnnDef TopModulePath NameId
  | AnnRef TopModulePath NameId

instance HasNameKindAnn Ann where
  annNameKind = AnnKind

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnUnkindedSym -> mempty
  AnnKeyword -> colorDull Blue
  AnnCode -> bold
  AnnImportant -> bold
  AnnComment -> colorDull Cyan
  AnnDelimiter -> colorDull White
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
  AnnDef {} -> mempty
  AnnRef {} -> mempty
