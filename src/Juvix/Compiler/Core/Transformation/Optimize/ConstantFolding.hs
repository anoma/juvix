module Juvix.Compiler.Core.Transformation.Optimize.ConstantFolding (constantFolding, constantFolding') where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Evaluator
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.FreeVarsInfo as Info
import Juvix.Compiler.Core.Transformation.Base

convertNode :: HashSet Symbol -> InfoTable -> Node -> Node
convertNode nonRecSyms tab = umap go
  where
    go :: Node -> Node
    go node = case node of
      NBlt BuiltinApp {..}
        | Info.isClosed node
            && _builtinAppOp /= OpFail
            && _builtinAppOp /= OpTrace
            && all isNonRecValue _builtinAppArgs ->
            doEval node
      NApp {}
        | Info.isClosed node ->
            case h of
              NIdt Ident {..}
                | HashSet.member _identSymbol nonRecSyms
                    && length args == ii ^. identifierArgsNum
                    && length tyargs == ii ^. identifierArgsNum
                    && isFirstOrderType tab tgt
                    && all isNonRecValue args ->
                    doEval node
                where
                  ii = lookupIdentifierInfo tab _identSymbol
                  (tyargs, tgt) = unfoldPi' (ii ^. identifierType)
              _ -> node
        where
          (h, args) = unfoldApps' node
      _ -> node

    isNonRecValue :: Node -> Bool
    isNonRecValue node = case node of
      NCst {} -> True
      NIdt Ident {..} ->
        HashSet.member _identSymbol nonRecSyms && isFirstOrderType tab ty
        where
          ty = lookupIdentifierInfo tab _identSymbol ^. identifierType
      NCtr Constr {..} -> all isNonRecValue _constrArgs
      NApp {} ->
        let (h, args) = unfoldApps' node
         in isNonRecValue h && all isType' args
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
constantFolding' nonRecSyms tab =
  mapAllNodes
    ( removeInfo kFreeVarsInfo
        . convertNode nonRecSyms tab
        . computeFreeVarsInfo
    )
    tab

constantFolding :: InfoTable -> InfoTable
constantFolding tab = constantFolding' (nonRecursiveIdents tab) tab
