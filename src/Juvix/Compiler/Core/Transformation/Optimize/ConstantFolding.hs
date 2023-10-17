module Juvix.Compiler.Core.Transformation.Optimize.ConstantFolding (constantFolding, constantFolding') where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Evaluator
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.FreeVarsInfo as Info
import Juvix.Compiler.Core.Transformation.Base

convertNode :: HashSet Symbol -> InfoTable -> Node -> Node
convertNode recSyms tab = umap go
  where
    go :: Node -> Node
    go node = case node of
      NBlt {}
        | Info.isClosed node ->
            doEval node
      NApp {}
        | Info.isClosed node ->
            let (h, args) = unfoldApps' node
             in case h of
                  NIdt Ident {..}
                    | not (HashSet.member _identSymbol recSyms)
                        && all isNonRecValue args ->
                        doEval node
                  _ -> node
      _ -> node

    isNonRecValue :: Node -> Bool
    isNonRecValue node = case node of
      NCst {} -> True
      NIdt Ident {..} ->
        not (HashSet.member _identSymbol recSyms) && isFirstOrderType tab ty
        where
          ty = lookupIdentifierInfo tab _identSymbol ^. identifierType
      NCtr Constr {..} -> all isNonRecValue _constrArgs
      _ -> isType' node

    doEval :: Node -> Node
    doEval = geval opts stderr (tab ^. identContext) []
      where
        opts =
          defaultEvalOptions
            { _evalOptionsNoFailure = True,
              _evalOptionsSilent = True
            }

constantFolding' :: HashSet Symbol -> InfoTable -> InfoTable
constantFolding' recSyms tab =
  mapAllNodes
    ( removeInfo kFreeVarsInfo
        . convertNode recSyms tab
        . computeFreeVarsInfo
    )
    tab

constantFolding :: InfoTable -> InfoTable
constantFolding tab = constantFolding' (recursiveIdents tab) tab
