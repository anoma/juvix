module Juvix.Compiler.Nockma.Highlight.Doc (nockOpDoc) where

import Juvix.Compiler.Nockma.Highlight.Doc.Base
import Juvix.Compiler.Nockma.Highlight.Doc.Parser
import Juvix.Compiler.Nockma.Highlight.Doc.Pretty ()
import Juvix.Data.CodeAnn
import Juvix.Prelude

example :: Rules
example =
  [rules|
    t * t => t
    ---
    s * p => index(s; p)
    |]

nockOpDoc :: NockOp -> Doc CodeAnn
nockOpDoc n = ppCodeAnn $ case n of
  OpAddress -> example
  OpQuote -> example
  OpApply -> example
  OpIsCell -> example
  OpInc -> example
  OpEq -> example
  OpIf -> example
  OpSequence -> example
  OpPush -> example
  OpCall -> example
  OpReplace -> example
  OpHint -> example
  OpScry -> example
