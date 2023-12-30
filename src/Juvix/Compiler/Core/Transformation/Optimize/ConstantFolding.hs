module Juvix.Compiler.Core.Transformation.Optimize.ConstantFolding (constantFolding, constantFolding') where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Evaluator
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.FreeVarsInfo as Info
import Juvix.Compiler.Core.Transformation.Base

convertNode :: HashSet Symbol -> InfoTable -> Module -> Node -> Node
convertNode nonRecSyms tab md = umap go
  where
    go :: Node -> Node
    go node = case node of
      NBlt BuiltinApp {..}
        | Info.isClosed node
            && _builtinAppOp /= OpFail
            && _builtinAppOp /= OpTrace
            && _builtinAppOp /= OpSeq
            && all isNonRecValue _builtinAppArgs ->
            doEval' node
      NApp {}
        | Info.isClosed node ->
            case h of
              NIdt Ident {..}
                | HashSet.member _identSymbol nonRecSyms
                    && evalAllowed
                    && length args == ii ^. identifierArgsNum
                    && length tyargs == ii ^. identifierArgsNum
                    && isZeroOrderType md tgt'
                    && all isNonRecValue args ->
                    doEval' node
                where
                  ii = lookupIdentifierInfo md _identSymbol
                  evalAllowed = maybe True (^. pragmaEval) (ii ^. identifierPragmas . pragmasEval)
                  (tyargs, tgt) = unfoldPi' (ii ^. identifierType)
                  n = length (takeWhile (isTypeConstr md) tyargs)
                  tys = reverse (take n args)
                  tgt' = substs tys (shift (-(length tyargs - n)) tgt)
              _ -> node
        where
          (h, args) = unfoldApps' node
      _ -> node

    isNonRecValue :: Node -> Bool
    isNonRecValue node = case node of
      NCst {} -> True
      NIdt Ident {..} ->
        HashSet.member _identSymbol nonRecSyms
      NCtr Constr {..} -> all isNonRecValue _constrArgs
      NApp {} ->
        let (h, args) = unfoldApps' node
         in isNonRecValue h && all isType' args
      _ -> isType' node

    doEval' :: Node -> Node
    doEval' = removeClosures . geval opts stderr (tab ^. identContext) []
      where
        opts =
          defaultEvalOptions
            { _evalOptionsNoFailure = True,
              _evalOptionsSilent = True
            }

constantFolding' :: HashSet Symbol -> InfoTable -> Module -> Module
constantFolding' nonRecSyms tab md =
  mapAllNodes
    ( removeInfo kFreeVarsInfo
        . convertNode nonRecSyms tab md
        . computeFreeVarsInfo
    )
    md

constantFolding :: Module -> Module
constantFolding md = constantFolding' (nonRecursiveIdents' tab) tab md
  where
    tab = computeCombinedInfoTable md
