module Juvix.Compiler.Core.Transformation.MoveApps
  ( moveApps,
    module Juvix.Compiler.Core.Transformation.Base,
  )
where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Transformation.Base

convertNode :: Node -> Node
convertNode = dmap go
  where
    go :: Node -> Node
    go node = case node of
      NApp {} ->
        let (tgt, args) = unfoldApps node
         in case tgt of
              NLet lt@(Let {..}) ->
                NLet lt {_letBody = mkApps _letBody (map (second (shift 1)) args)}
              NCase cs@(Case {..}) ->
                NCase
                  cs
                    { _caseBranches =
                        map
                          ( \br@CaseBranch {..} ->
                              br
                                { _caseBranchBody = mkApps _caseBranchBody (map (second (shift _caseBranchBindersNum)) args)
                                }
                          )
                          _caseBranches,
                      _caseDefault = fmap (`mkApps` args) _caseDefault
                    }
              NBlt blt@BuiltinApp {..}
                | _builtinAppOp == OpSeq ->
                    case _builtinAppArgs ++ map snd args of
                      [arg1] -> NBlt blt {_builtinAppArgs = [arg1]}
                      arg1 : arg2 : args' -> NBlt blt {_builtinAppArgs = [arg1, mkApps' arg2 args']}
                      _ -> impossible
              NBlt BuiltinApp {..}
                | _builtinAppOp == OpFail ->
                    tgt
              NBlt blt@BuiltinApp {..}
                | _builtinAppOp == OpTrace ->
                    case _builtinAppArgs ++ map snd args of
                      [arg] -> NBlt blt {_builtinAppArgs = [arg]}
                      arg : args' ->
                        mkLet'
                          mkDynamic'
                          (NBlt blt {_builtinAppArgs = [arg]})
                          (mkApps' (mkVar' 0) (map (shift 1) args'))
                      _ -> impossible
              _ -> node
      _ -> node

moveApps :: InfoTable -> InfoTable
moveApps tab = mapT (const convertNode) tab
