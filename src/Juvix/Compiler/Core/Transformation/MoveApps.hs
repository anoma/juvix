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
        in
        case tgt of
          NLet lt@(Let {..}) ->
            NLet lt{_letBody = mkApps _letBody (map (second (shift 1)) args)}
          NCase cs@(Case {..}) ->
            NCase cs{
              _caseBranches =
                map (\br@CaseBranch{..} ->
                    br{
                      _caseBranchBody = mkApps _caseBranchBody (map (second (shift _caseBranchBindersNum)) args)
                      })
                    _caseBranches,
              _caseDefault = fmap (`mkApps` args) _caseDefault
            }
          _ -> node
      _ -> node

moveApps :: InfoTable -> InfoTable
moveApps tab = mapT (const convertNode) tab
