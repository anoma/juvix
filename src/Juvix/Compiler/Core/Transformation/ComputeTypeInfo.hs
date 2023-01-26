module Juvix.Compiler.Core.Transformation.ComputeTypeInfo (computeTypeInfo) where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.TypeInfo qualified as Info
import Juvix.Compiler.Core.Transformation.Base

-- | Computes the TypeInfo for each subnode.
--
-- Assumptions:
-- 1. No polymorphism.
-- 2. No dynamic type.
-- 3. All binders and identifiers are decorated with full type information.
-- 4. All cases have at least one branch.
-- 5. No `Match` nodes.
-- 6. All inductives and function types are in universe 0.
computeNodeTypeInfo :: InfoTable -> Node -> Node
computeNodeTypeInfo tab = umapL go
  where
    go :: BinderList Binder -> Node -> Node
    go bl node = Info.setNodeType (nodeType bl node) node

    nodeType :: BinderList Binder -> Node -> Type
    nodeType bl = \case
      NVar Var {..} ->
        BL.lookup _varIndex bl ^. binderType
      NIdt Ident {..} ->
        fromJust (HashMap.lookup _identSymbol (tab ^. infoIdentifiers)) ^. identifierType
      NCst Constant {..} ->
        case _constantValue of
          ConstInteger {} -> mkTypeInteger'
          ConstString {} -> mkTypeString'
      NApp App {..} ->
        let lty = Info.getNodeType _appLeft
            rty = Info.getNodeType _appRight
         in case lty of
              NPi Pi {..}
                | rty == _piBinder ^. binderType ->
                    _piBody
              _ ->
                error "incorrect type information (application)"
      NBlt BuiltinApp {..} ->
        case _builtinAppOp of
          OpIntAdd -> mkTypeInteger'
          OpIntSub -> mkTypeInteger'
          OpIntMul -> mkTypeInteger'
          OpIntDiv -> mkTypeInteger'
          OpIntMod -> mkTypeInteger'
          OpIntLt -> mkTypeBool'
          OpIntLe -> mkTypeBool'
          OpEq -> mkTypeBool'
          OpTrace -> case _builtinAppArgs of
            [_, arg2] -> Info.getNodeType arg2
            _ -> error "incorrect trace builtin application"
          OpFail -> mkDynamic'
      NCtr Constr {..} ->
        let ci = fromJust $ HashMap.lookup _constrTag (tab ^. infoConstructors)
            ii = fromJust $ HashMap.lookup (ci ^. constructorInductive) (tab ^. infoInductives)
         in mkTypeConstr' (ci ^. constructorInductive) (take (length (ii ^. inductiveParams)) _constrArgs)
      NLam Lambda {..} ->
        mkPi' (_lambdaBinder ^. binderType) (mkLambda' (Info.getNodeType _lambdaBody))
      NLet Let {..} ->
        Info.getNodeType _letBody
      NRec LetRec {..} ->
        Info.getNodeType _letRecBody
      NCase Case {..} -> case _caseDefault of
        Just nd -> Info.getNodeType nd
        Nothing -> case _caseBranches of
          CaseBranch {..} : _ ->
            Info.getNodeType _caseBranchBody
          [] -> error "case with no branches"
      NMatch Match {} ->
        error "match unsupported"
      NPi Pi {} ->
        mkUniv' 0
      NUniv Univ {..} ->
        mkUniv' (_univLevel + 1)
      NTyp TypeConstr {} ->
        mkUniv' 0
      NPrim TypePrim {} ->
        mkUniv' 0
      NDyn Dynamic {} ->
        mkUniv' 0
      Closure {} ->
        impossible

computeTypeInfo :: InfoTable -> InfoTable
computeTypeInfo tab = mapT (const (computeNodeTypeInfo tab)) tab
