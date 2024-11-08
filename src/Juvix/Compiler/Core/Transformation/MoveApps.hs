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
                NLet lt {_letBody = mkApps _letBody (map (second (shift "movap" 1)) args)}
              NCase cs@(Case {..}) ->
                NCase
                  cs
                    { _caseBranches =
                        map
                          ( \br@CaseBranch {..} ->
                              br
                                { _caseBranchBody = mkApps _caseBranchBody (map (second (shift "nested" _caseBranchBindersNum)) args)
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
                          (mkApps' (mkVar' 0) (map (shift "builtinf" 1) args'))
                      _ -> impossible
              _ -> node
      _ -> node

-- | Move applications inside let bindings and case branches.
--
-- The translation from Core to Core.Stripped requires that only variables or
-- function symbols be present at the head of an application.
--
-- We transform
--
-- `(let x := M in N) Q`
--
-- to
--
-- `let x := M in N Q`
--
-- and e.g.
--
-- `(case M of { c1 x := N1; c2 x y := N2 }) Q`
--
-- to
--
-- `case M of { c1 x := N1 Q; c2 x y := N2 Q }`
--
-- References:
--  - https://github.com/anoma/juvix/issues/1654
--  - https://github.com/anoma/juvix/pull/1659
moveApps :: Module -> Module
moveApps = mapAllNodes convertNode
