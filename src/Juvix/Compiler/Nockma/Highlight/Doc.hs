module Juvix.Compiler.Nockma.Highlight.Doc (nockOpDoc) where

import Juvix.Compiler.Nockma.Language
import Juvix.Data.CodeAnn

stack :: Doc CodeAnn
stack = annotate (AnnKind KNameAxiom) "𝒮"

-- term1 :: Doc CodeAnn
-- term1 = annotate AnnJudoc "𝓉₁"

-- term2 :: Doc CodeAnn
-- term2 = annotate AnnLiteralString "𝓉₂"

-- term3 :: Doc CodeAnn
-- term3 = annotate AnnLiteralInteger "𝓉₃"

path1 :: Doc CodeAnn
path1 = annotate (AnnKind KNameFunction) "𝓅"

evaluatesTo :: Doc CodeAnn
evaluatesTo = annotate AnnKeyword "⇒"

indexOp :: Doc CodeAnn
indexOp = annotate AnnKeyword "!"

nockOpDoc :: NockOp -> Doc CodeAnn
nockOpDoc = \case
  OpAddress ->
    stack <> ","
      <+> ppCodeAnn OpAddress
      <+> path1
      <+> evaluatesTo
      <+> stack <> indexOp <> path1
  OpQuote -> "OpQuote"
  OpApply -> "OpApply"
  OpIsCell -> "OpIsCell"
  OpInc -> "OpInc"
  OpEq -> "OpEq"
  OpIf -> "OpIf"
  OpSequence -> "OpSequence"
  OpPush -> "OpPush"
  OpCall -> "OpCall"
  OpReplace -> "OpReplace"
  OpHint -> "OpHint"
  OpScry -> "OpScry"
