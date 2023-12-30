module Juvix.Compiler.Core.Transformation.Optimize.SimplifyIfs (simplifyIfs, simplifyIfs') where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Bool -> Module -> Node -> Node
convertNode bFast md = umap go
  where
    boolSym = lookupConstructorInfo md (BuiltinTag TagTrue) ^. constructorInductive

    go :: Node -> Node
    go node = case node of
      NCase c@Case {..}
        | isCaseBoolean _caseBranches ->
            translateCaseIf goIf c
      _ -> node

    goIf :: Node -> Node -> Node -> Node
    goIf v b1 b2
      | isTrueConstr b1 && isFalseConstr b2 = v
      | bFast && isTrueConstr b1 && isTrueConstr b2 = b1
      | bFast && isFalseConstr b1 && isFalseConstr b2 = b1
      | not bFast && b1 == b2 = b1
      | otherwise = mkIf' boolSym v b1 b2

simplifyIfs' :: Bool -> Module -> Module
simplifyIfs' bFast md = mapAllNodes (convertNode bFast md) md

simplifyIfs :: Module -> Module
simplifyIfs = simplifyIfs' False
