module Juvix.Compiler.Core.Transformation.ComputeTypeInfo where

import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.TypeInfo qualified as Info
import Juvix.Compiler.Core.Transformation.Base

computeNodeType :: InfoTable -> Node -> Type
computeNodeType tab = Info.getNodeType . computeNodeTypeInfo tab

-- | Computes the TypeInfo for each subnode of a well-typed node.
--
-- Assumptions:
-- 1. The node is well-typed.
-- 2. All binders and identifiers are decorated with correct full type
--    information.
-- 3. All cases have at least one branch.
-- 4. No `Match` nodes.
-- 5. All inductives and function types are in universe 0.
computeNodeTypeInfo :: InfoTable -> Node -> Node
computeNodeTypeInfo tab = umapL go
  where
    go :: BinderList Binder -> Node -> Node
    go bl node = Info.setNodeType (nodeType bl node) node

    nodeType :: BinderList Binder -> Node -> Type
    nodeType bl node = case node of
      NVar Var {..} ->
        shift (_varIndex + 1) (BL.lookup _varIndex bl ^. binderType)
      NIdt Ident {..} ->
        lookupIdentifierInfo tab _identSymbol ^. identifierType
      NCst Constant {..} ->
        case _constantValue of
          ConstInteger {} -> mkTypeInteger'
          ConstString {} -> mkTypeString'
      NApp {} ->
        let (fn, args) = unfoldApps' node
            fty = Info.getNodeType fn
            (tyargs, target) = unfoldPi fty
            target' = rePis (drop (length args) tyargs) target
         in substs (reverse args) target'
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
          OpShow -> mkTypeString'
          OpStrConcat -> mkTypeString'
          OpStrToInt -> mkTypeInteger'
          OpSeq -> case _builtinAppArgs of
            [_, arg2] -> Info.getNodeType arg2
            _ -> error "incorrect seq builtin application"
          OpTrace -> case _builtinAppArgs of
            [arg] -> Info.getNodeType arg
            _ -> error "incorrect trace builtin application"
          OpFail -> Info.getNodeType node
      NCtr Constr {..} ->
        let ci = lookupConstructorInfo tab _constrTag
            ii = lookupInductiveInfo tab (ci ^. constructorInductive)
         in case ii ^. inductiveBuiltin of
              Just (BuiltinTypeInductive BuiltinBool) ->
                mkTypeBool'
              _ ->
                mkTypeConstr' (ci ^. constructorInductive) (take (length (ii ^. inductiveParams)) _constrArgs)
      NLam Lambda {..} ->
        mkPi mempty _lambdaBinder (Info.getNodeType _lambdaBody)
      NLet Let {..} ->
        Info.getNodeType _letBody
      NRec LetRec {..} ->
        Info.getNodeType _letRecBody
      NCase Case {..} -> case _caseDefault of
        Just nd -> Info.getNodeType nd
        Nothing -> case _caseBranches of
          CaseBranch {..} : _ ->
            shift (-_caseBranchBindersNum) (Info.getNodeType _caseBranchBody)
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
      NBot Bottom {..} ->
        _bottomType
      Closure {} ->
        impossible

computeTypeInfo :: InfoTable -> InfoTable
computeTypeInfo tab = mapT (const (computeNodeTypeInfo tab)) tab
