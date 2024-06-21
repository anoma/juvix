module Juvix.Compiler.Tree.Extra.Rep where

import Juvix.Compiler.Tree.Data.InfoTable.Base
import Juvix.Compiler.Tree.Language.Base

isRecord :: InductiveInfo -> Bool
isRecord InductiveInfo {..} = length _inductiveConstructors == 1

isInductiveRecord :: InfoTable' a e -> Symbol -> Bool
isInductiveRecord tab sym = isRecord (lookupInductiveInfo tab sym)

isConstrRecord :: InfoTable' a e -> Tag -> Bool
isConstrRecord tab tag =
  isInductiveRecord tab (lookupConstrInfo tab tag ^. constructorInductive)
