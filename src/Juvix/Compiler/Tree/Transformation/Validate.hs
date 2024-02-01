module Juvix.Compiler.Tree.Transformation.Validate where

import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Tree.Error
import Juvix.Compiler.Tree.Extra.Base (getNodeLocation)
import Juvix.Compiler.Tree.Extra.Recursors
import Juvix.Compiler.Tree.Extra.Type
import Juvix.Compiler.Tree.Transformation.Base

inferType :: forall r. (Member (Error TreeError) r) => InfoTable -> FunctionInfo -> Node -> Sem r Type
inferType tab funInfo = goInfer mempty
  where
    goInfer :: BinderList Type -> Node -> Sem r Type
    goInfer bl = \case
      Binop x -> goBinop bl x
      Unop x -> goUnop bl x
      Const x -> goConst bl x
      MemRef x -> goMemRef bl x
      AllocConstr x -> goAllocConstr bl x
      AllocClosure x -> goAllocClosure bl x
      ExtendClosure x -> goExtendClosure bl x
      Call x -> goCall bl x
      CallClosures x -> goCallClosures bl x
      Branch x -> goBranch bl x
      Case x -> goCase bl x
      Save x -> goSave bl x

    goBinop :: BinderList Type -> NodeBinop -> Sem r Type
    goBinop bl NodeBinop {..} = case _nodeBinopOpcode of
      IntAdd -> checkBinop mkTypeInteger mkTypeInteger mkTypeInteger
      IntSub -> checkBinop mkTypeInteger mkTypeInteger mkTypeInteger
      IntMul -> checkBinop mkTypeInteger mkTypeInteger mkTypeInteger
      IntDiv -> checkBinop mkTypeInteger mkTypeInteger mkTypeInteger
      IntMod -> checkBinop mkTypeInteger mkTypeInteger mkTypeInteger
      IntLt -> checkBinop mkTypeInteger mkTypeInteger mkTypeBool
      IntLe -> checkBinop mkTypeInteger mkTypeInteger mkTypeBool
      ValEq -> checkBinop TyDynamic TyDynamic mkTypeBool
      StrConcat -> checkBinop TyString TyString TyString
      OpSeq -> do
        checkType bl _nodeBinopArg1 TyDynamic
        goInfer bl _nodeBinopArg2
      where
        loc = _nodeBinopInfo ^. nodeInfoLocation

        checkBinop :: Type -> Type -> Type -> Sem r Type
        checkBinop ty1' ty2' rty = do
          ty1 <- goInfer bl _nodeBinopArg1
          ty2 <- goInfer bl _nodeBinopArg2
          _ <- unifyTypes' loc tab ty1 ty1'
          _ <- unifyTypes' loc tab ty2 ty2'
          return rty

    goUnop :: BinderList Type -> NodeUnop -> Sem r Type
    goUnop bl NodeUnop {..} = case _nodeUnopOpcode of
      OpShow -> checkUnop TyDynamic TyString
      OpStrToInt -> checkUnop TyString mkTypeInteger
      OpTrace -> goInfer bl _nodeUnopArg
      OpFail -> checkUnop TyDynamic TyDynamic
      OpArgsNum -> checkUnop TyDynamic mkTypeInteger
      where
        loc = _nodeUnopInfo ^. nodeInfoLocation

        checkUnop :: Type -> Type -> Sem r Type
        checkUnop ty rty = do
          ty' <- goInfer bl _nodeUnopArg
          _ <- unifyTypes' loc tab ty ty'
          return rty

    goConst :: BinderList Type -> NodeConstant -> Sem r Type
    goConst _ NodeConstant {..} = case _nodeConstant of
      ConstInt {} -> return mkTypeInteger
      ConstBool {} -> return mkTypeBool
      ConstString {} -> return TyString
      ConstUnit {} -> return TyUnit
      ConstVoid {} -> return TyVoid

    goMemRef :: BinderList Type -> NodeMemRef -> Sem r Type
    goMemRef bl NodeMemRef {..} = case _nodeMemRef of
      DRef d -> goDirectRef (_nodeMemRefInfo ^. nodeInfoLocation) bl d
      ConstrRef x -> goField bl x

    goDirectRef :: Maybe Location -> BinderList Type -> DirectRef -> Sem r Type
    goDirectRef loc bl = \case
      ArgRef x -> goArgRef loc bl x
      TempRef RefTemp {..} -> goTempRef bl _refTempOffsetRef

    goArgRef :: Maybe Location -> BinderList Type -> OffsetRef -> Sem r Type
    goArgRef loc _ OffsetRef {..}
      | _offsetRefOffset < length tys = return $ tys !! _offsetRefOffset
      | typeTarget (funInfo ^. functionType) == TyDynamic = return TyDynamic
      | otherwise =
          throw $
            TreeError
              { _treeErrorLoc = loc,
                _treeErrorMsg = "Wrong target type"
              }
      where
        tys = typeArgs (funInfo ^. functionType)

    goTempRef :: BinderList Type -> OffsetRef -> Sem r Type
    goTempRef bl OffsetRef {..} = return $ BL.lookupLevel _offsetRefOffset bl

    goField :: BinderList Type -> Field -> Sem r Type
    goField _ Field {..}
      | _fieldOffset < length tys = return $ tys !! _fieldOffset
      | otherwise = return TyDynamic
      where
        ci = lookupConstrInfo tab _fieldTag
        tys = typeArgs (ci ^. constructorType)

    goAllocConstr :: BinderList Type -> NodeAllocConstr -> Sem r Type
    goAllocConstr bl NodeAllocConstr {..}
      | length _nodeAllocConstrArgs == length tys = do
          forM_ (zipExact _nodeAllocConstrArgs tys) (uncurry (checkType bl))
          return $ typeTarget (ci ^. constructorType)
      | otherwise =
          throw $
            TreeError
              { _treeErrorLoc = _nodeAllocConstrInfo ^. nodeInfoLocation,
                _treeErrorMsg = ""
              }
      where
        ci = lookupConstrInfo tab _nodeAllocConstrTag
        tys = typeArgs (ci ^. constructorType)

    goAllocClosure :: BinderList Type -> NodeAllocClosure -> Sem r Type
    goAllocClosure bl NodeAllocClosure {..}
      | n <= fi ^. functionArgsNum = do
          forM_ (zipExact _nodeAllocClosureArgs (take n tys)) (uncurry (checkType bl))
          return $ mkTypeFun (drop n tys) (typeTarget (fi ^. functionType))
      | otherwise =
          throw $
            TreeError
              { _treeErrorLoc = _nodeAllocClosureInfo ^. nodeInfoLocation,
                _treeErrorMsg = "Wrong number of arguments"
              }
      where
        n = length _nodeAllocClosureArgs
        fi = lookupFunInfo tab _nodeAllocClosureFunSymbol
        tys = typeArgs (fi ^. functionType)

    goExtendClosure :: BinderList Type -> NodeExtendClosure -> Sem r Type
    goExtendClosure bl NodeExtendClosure {..} = do
      ty <- goInfer bl _nodeExtendClosureFun
      let tys = typeArgs ty
          m = length tys
          n = length _nodeExtendClosureArgs
      if
          | n < m -> do
              forM_ (zipExact (toList _nodeExtendClosureArgs) (take n tys)) (uncurry (checkType bl))
              return $ mkTypeFun (drop n tys) (typeTarget ty)
          | typeTarget ty == TyDynamic -> do
              let tys' = tys ++ replicate (n - m) TyDynamic
              forM_ (zipExact (toList _nodeExtendClosureArgs) tys') (uncurry (checkType bl))
              return $ typeTarget ty
          | otherwise ->
              throw $
                TreeError
                  { _treeErrorLoc = _nodeExtendClosureInfo ^. nodeInfoLocation,
                    _treeErrorMsg = "Too many arguments"
                  }

    goCall :: BinderList Type -> NodeCall -> Sem r Type
    goCall bl NodeCall {..} = case _nodeCallType of
      CallFun sym
        | n == fi ^. functionArgsNum -> do
            unless (n == 0) $
              forM_ (zipExact _nodeCallArgs tys) (uncurry (checkType bl))
            return $ mkTypeFun (drop n tys) (typeTarget (fi ^. functionType))
        | otherwise ->
            throw $
              TreeError
                { _treeErrorLoc = _nodeCallInfo ^. nodeInfoLocation,
                  _treeErrorMsg = "Wrong number of arguments"
                }
        where
          n = length _nodeCallArgs
          fi = lookupFunInfo tab sym
          tys = typeArgs (fi ^. functionType)
      CallClosure cl -> do
        ty <- goInfer bl cl
        let tys = typeArgs ty
            n = length _nodeCallArgs
        when (length tys > n) $
          throw $
            TreeError
              { _treeErrorLoc = _nodeCallInfo ^. nodeInfoLocation,
                _treeErrorMsg = "Too few arguments"
              }
        when (length tys < n && typeTarget ty /= TyDynamic) $
          throw $
            TreeError
              { _treeErrorLoc = _nodeCallInfo ^. nodeInfoLocation,
                _treeErrorMsg = "Too many arguments"
              }
        let tys' = tys ++ replicate (n - length tys) TyDynamic
        forM_ (zipExact _nodeCallArgs tys') (uncurry (checkType bl))
        return $ typeTarget ty

    goCallClosures :: BinderList Type -> NodeCallClosures -> Sem r Type
    goCallClosures bl NodeCallClosures {..} = do
      ty <- goInfer bl _nodeCallClosuresFun
      go ty (toList _nodeCallClosuresArgs)
      where
        go :: Type -> [Node] -> Sem r Type
        go ty args
          | m == 0 =
              return ty
          | m <= n = do
              forM_ (zipExact (take m args) tys) (uncurry (checkType bl))
              go (typeTarget ty) (drop m args)
          | otherwise = do
              forM_ (zipExact args (take n tys)) (uncurry (checkType bl))
              return $ mkTypeFun (drop n tys) (typeTarget ty)
          where
            tys = typeArgs ty
            m = length tys
            n = length args

    goBranch :: BinderList Type -> NodeBranch -> Sem r Type
    goBranch bl NodeBranch {..} = do
      checkType bl _nodeBranchArg mkTypeBool
      ty1 <- goInfer bl _nodeBranchTrue
      ty2 <- goInfer bl _nodeBranchFalse
      unifyTypes' (_nodeBranchInfo ^. nodeInfoLocation) tab ty1 ty2

    goCase :: BinderList Type -> NodeCase -> Sem r Type
    goCase bl NodeCase {..} = do
      ity <- goInfer bl _nodeCaseArg
      unless (ity == mkTypeInductive _nodeCaseInductive || ity == TyDynamic) $
        throw $
          TreeError
            { _treeErrorLoc = _nodeCaseInfo ^. nodeInfoLocation,
              _treeErrorMsg = "Inductive type mismatch"
            }
      ty <- maybe (return TyDynamic) (goInfer bl) _nodeCaseDefault
      go ity ty _nodeCaseBranches
      where
        go :: Type -> Type -> [CaseBranch] -> Sem r Type
        go ity ty = \case
          [] -> return ty
          CaseBranch {..} : brs -> do
            let bl' = if _caseBranchSave then BL.cons ity bl else bl
            ty' <- goInfer bl' _caseBranchBody
            ty'' <- unifyTypes' (_nodeCaseInfo ^. nodeInfoLocation) tab ty ty'
            go ity ty'' brs

    goSave :: BinderList Type -> NodeSave -> Sem r Type
    goSave bl NodeSave {..} = do
      ty <- goInfer bl _nodeSaveArg
      goInfer (BL.cons ty bl) _nodeSaveBody

    checkType :: BinderList Type -> Node -> Type -> Sem r ()
    checkType bl node ty = do
      ty' <- goInfer bl node
      _ <- unifyTypes' (getNodeLocation node) tab ty ty'
      return ()

validateFunction :: (Member (Error TreeError) r) => InfoTable -> FunctionInfo -> Sem r FunctionInfo
validateFunction tab funInfo = do
  ty <- inferType tab funInfo (funInfo ^. functionCode)
  let ty' = if funInfo ^. functionArgsNum == 0 then funInfo ^. functionType else typeTarget (funInfo ^. functionType)
  _ <- unifyTypes' (funInfo ^. functionLocation) tab ty ty'
  return funInfo

validate :: (Member (Error TreeError) r) => InfoTable -> Sem r InfoTable
validate tab = mapFunctionsM (validateFunction tab) tab
