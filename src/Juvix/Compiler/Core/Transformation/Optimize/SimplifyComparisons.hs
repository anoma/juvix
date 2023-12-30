module Juvix.Compiler.Core.Transformation.Optimize.SimplifyComparisons (simplifyComparisons) where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Module -> Node -> Node
convertNode md = dmap go
  where
    boolSym = lookupConstructorInfo md (BuiltinTag TagTrue) ^. constructorInductive

    go :: Node -> Node
    go node = case node of
      NCase c@Case {..}
        | isCaseBoolean _caseBranches ->
            translateCaseIf goIf c
      _ -> node

    goIf :: Node -> Node -> Node -> Node
    goIf v b1 b2 = case v of
      NBlt blt@BuiltinApp {..}
        | OpEq <- _builtinAppOp ->
            case b2 of
              NCase c@Case {..}
                | isCaseBoolean _caseBranches ->
                    translateCaseIf (goCmp v b1) c
              NBlt blt'
                | OpIntLt <- blt' ^. builtinAppOp,
                  blt ^. builtinAppArgs == blt' ^. builtinAppArgs ->
                    if
                        | isFalseConstr b1 ->
                            b2
                        | isTrueConstr b1 ->
                            NBlt blt {_builtinAppOp = OpIntLe}
                        | otherwise ->
                            mkIf' boolSym v b1 b2
              _ ->
                mkIf' boolSym v b1 b2
        | OpIntLt <- _builtinAppOp,
          isFalseConstr b1 && isTrueConstr b2 ->
            NBlt
              blt
                { _builtinAppOp = OpIntLe,
                  _builtinAppArgs = reverse _builtinAppArgs
                }
        | OpIntLe <- _builtinAppOp,
          isFalseConstr b1 && isTrueConstr b2 ->
            NBlt
              blt
                { _builtinAppOp = OpIntLt,
                  _builtinAppArgs = reverse _builtinAppArgs
                }
      _ ->
        mkIf' boolSym v b1 b2

    goCmp :: Node -> Node -> Node -> Node -> Node -> Node
    goCmp v b1 v' b1' b2' = case (v, v') of
      (NBlt blt, NBlt blt')
        | (OpEq, OpIntLt) <- (blt ^. builtinAppOp, blt' ^. builtinAppOp),
          blt ^. builtinAppArgs == blt' ^. builtinAppArgs ->
            if
                | isFalseConstr b1 && isTrueConstr b1' && isFalseConstr b2' ->
                    v'
                | isTrueConstr b1 && isFalseConstr b1' && isFalseConstr b2' ->
                    v
                | isTrueConstr b1 && isTrueConstr b1' && isFalseConstr b2' ->
                    NBlt blt {_builtinAppOp = OpIntLe}
                | isFalseConstr b1 && isFalseConstr b1' && isTrueConstr b2' ->
                    NBlt
                      blt
                        { _builtinAppOp = OpIntLt,
                          _builtinAppArgs = reverse (blt ^. builtinAppArgs)
                        }
                | isTrueConstr b1 && isFalseConstr b1' && isTrueConstr b2' ->
                    NBlt
                      blt
                        { _builtinAppOp = OpIntLe,
                          _builtinAppArgs = reverse (blt ^. builtinAppArgs)
                        }
                | isFalseConstr b1 && isTrueConstr b1' && isTrueConstr b2' ->
                    mkIf' boolSym v b1 b1'
                | b1 == b2' ->
                    mkIf' boolSym v' b1' b2'
                | b1' == b2' ->
                    mkIf' boolSym v b1 b1'
                | b1 == b1' ->
                    mkIf' boolSym (NBlt blt {_builtinAppOp = OpIntLe}) b1 b2'
                | otherwise ->
                    theIfs
      _ ->
        theIfs
      where
        theIfs = mkIf' boolSym v b1 (mkIf' boolSym v' b1' b2')

simplifyComparisons :: Module -> Module
simplifyComparisons md = mapAllNodes (convertNode md) md
