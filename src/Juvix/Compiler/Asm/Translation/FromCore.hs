module Juvix.Compiler.Asm.Translation.FromCore (fromCore) where

import Data.DList qualified as DL
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Extra.Type
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.Stripped.InfoTable qualified as Core
import Juvix.Compiler.Core.Language.Stripped qualified as Core

type BinderList = BL.BinderList

-- DList for O(1) snoc and append
type Code' = DL.DList Command

fromCore :: Core.InfoTable -> InfoTable
fromCore tab =
  InfoTable
    { _infoMainFunction = tab ^. Core.infoMain,
      _infoFunctions = fmap (genCode tab) (tab ^. Core.infoFunctions),
      _infoInductives = fmap translateInductiveInfo (tab ^. Core.infoInductives),
      _infoConstrs = fmap translateConstructorInfo (tab ^. Core.infoConstructors)
    }

-- Generate code for a single function.
genCode :: Core.InfoTable -> Core.FunctionInfo -> FunctionInfo
genCode infoTable fi =
  let code =
        DL.toList $
          go
            True
            0
            ( BL.fromList $
                reverse (map (Ref . DRef . ArgRef) [0 .. fi ^. Core.functionArgsNum - 1])
            )
            (fi ^. Core.functionBody)
   in FunctionInfo
        { _functionName = fi ^. Core.functionName,
          _functionLocation = fi ^. Core.functionLocation,
          _functionSymbol = fi ^. Core.functionSymbol,
          _functionArgsNum = fi ^. Core.functionArgsNum,
          _functionType = convertType (fi ^. Core.functionArgsNum) (fi ^. Core.functionType),
          _functionCode = code,
          _functionMaxTempStackHeight = -1, -- computed later
          _functionMaxValueStackHeight = -1
        }
  where
    -- Assumption: the BinderList does not contain references to the value stack
    -- (directly or indirectly).
    go :: Bool -> Int -> BinderList Value -> Core.Node -> Code'
    go isTail tempSize refs node = case node of
      Core.NVar v -> goVar isTail refs v
      Core.NIdt idt -> goIdent isTail idt
      Core.NCst cst -> goConstant isTail cst
      Core.NApp apps -> goApps isTail tempSize refs apps
      Core.NBlt blt -> goBuiltinApp isTail tempSize refs blt
      Core.NCtr ctr -> goConstr isTail tempSize refs ctr
      Core.NLet lt -> goLet isTail tempSize refs lt
      Core.NCase c -> goCase isTail tempSize refs c
      Core.NIf x -> goIf isTail tempSize refs x

    goVar :: Bool -> BinderList Value -> Core.Var -> Code'
    goVar isTail refs Core.Var {..} =
      snocReturn isTail $
        DL.singleton $
          mkInstr $
            Push (BL.lookup _varIndex refs)

    goIdent :: Bool -> Core.Ident -> Code'
    goIdent isTail Core.Ident {..} =
      if
          | getArgsNum _identSymbol == 0 ->
              DL.singleton $
                mkInstr $
                  (if isTail then TailCall else Call) (InstrCall (CallFun _identSymbol) 0)
          | otherwise ->
              snocReturn isTail $
                DL.singleton $
                  mkInstr $
                    AllocClosure (InstrAllocClosure _identSymbol 0)

    goConstant :: Bool -> Core.Constant -> Code'
    goConstant isTail = \case
      Core.Constant _ (Core.ConstInteger i) ->
        snocReturn isTail $
          DL.singleton $
            mkInstr $
              Push (ConstInt i)
      Core.Constant _ (Core.ConstString s) ->
        snocReturn isTail $
          DL.singleton $
            mkInstr $
              Push (ConstString s)

    goApps :: Bool -> Int -> BinderList Value -> Core.Apps -> Code'
    goApps isTail tempSize refs (Core.Apps {..}) =
      let suppliedArgs = reverse _appsArgs
          suppliedArgsNum = length suppliedArgs
       in case _appsFun of
            Core.FunIdent (Core.Ident {..}) ->
              if
                  | argsNum > suppliedArgsNum ->
                      snocReturn isTail $
                        DL.snoc
                          (DL.concat (map (go False tempSize refs) suppliedArgs))
                          (mkInstr $ AllocClosure (InstrAllocClosure _identSymbol suppliedArgsNum))
                  | argsNum == suppliedArgsNum ->
                      DL.snoc
                        (DL.concat (map (go False tempSize refs) suppliedArgs))
                        (mkInstr $ (if isTail then TailCall else Call) (InstrCall (CallFun _identSymbol) argsNum))
                  | otherwise ->
                      -- If more arguments are supplied (suppliedArgsNum) than
                      -- the function eats up (argsNum), then the function
                      -- returns a closure. We should first call the function
                      -- (with Call) and then use CallClosures or
                      -- TailCallClosures on the result with the remaining
                      -- arguments.
                      DL.snoc
                        ( DL.snoc
                            (DL.concat (map (go False tempSize refs) suppliedArgs))
                            (mkInstr $ Call (InstrCall (CallFun _identSymbol) argsNum))
                        )
                        (mkInstr $ (if isTail then TailCallClosures else CallClosures) (InstrCallClosures (suppliedArgsNum - argsNum)))
              where
                argsNum = getArgsNum _identSymbol
            Core.FunVar Core.Var {..} ->
              DL.snoc
                ( DL.snoc
                    (DL.concat (map (go False tempSize refs) suppliedArgs))
                    (mkInstr $ Push (BL.lookup _varIndex refs))
                )
                (mkInstr $ (if isTail then TailCallClosures else CallClosures) (InstrCallClosures suppliedArgsNum))

    goBuiltinApp :: Bool -> Int -> BinderList Value -> Core.BuiltinApp -> Code'
    goBuiltinApp isTail tempSize refs (Core.BuiltinApp {..}) =
      snocReturn isTail $
        DL.append
          (DL.concat (map (go False tempSize refs) (reverse _builtinAppArgs)))
          (genOp _builtinAppOp)

    goConstr :: Bool -> Int -> BinderList Value -> Core.Constr -> Code'
    goConstr isTail tempSize refs = \case
      Core.Constr _ (Core.BuiltinTag Core.TagTrue) _ ->
        snocReturn isTail $
          DL.singleton $
            mkInstr $
              Push (ConstBool True)
      Core.Constr _ (Core.BuiltinTag Core.TagFalse) _ ->
        snocReturn isTail $
          DL.singleton $
            mkInstr $
              Push (ConstBool False)
      Core.Constr {..} ->
        snocReturn isTail $
          DL.snoc
            (DL.concat (map (go False tempSize refs) (reverse _constrArgs)))
            (mkInstr $ AllocConstr _constrTag)

    goLet :: Bool -> Int -> BinderList Value -> Core.Let -> Code'
    goLet isTail tempSize refs (Core.Let {..}) =
      DL.append
        (DL.snoc (go False tempSize refs (_letItem ^. Core.letItemValue)) (mkInstr PushTemp))
        (snocPopTemp isTail $ go isTail (tempSize + 1) (BL.cons (Ref (DRef (TempRef tempSize))) refs) _letBody)

    goCase :: Bool -> Int -> BinderList Value -> Core.Case -> Code'
    goCase isTail tempSize refs (Core.Case {..}) =
      DL.snoc
        (go False tempSize refs _caseValue)
        ( Case $
            CmdCase
              { _cmdCaseInfo = emptyInfo,
                _cmdCaseInductive = _caseInductive,
                _cmdCaseBranches = compileCaseBranches _caseBranches,
                _cmdCaseDefault = fmap compileCaseDefault _caseDefault
              }
        )
      where
        compileCaseBranches :: [Core.CaseBranch] -> [CaseBranch]
        compileCaseBranches branches =
          map
            ( \(Core.CaseBranch {..}) ->
                if
                    | _caseBranchBindersNum == 0 ->
                        compileCaseBranchNoBinders _caseBranchTag _caseBranchBody
                    | otherwise ->
                        compileCaseBranch _caseBranchBindersNum _caseBranchTag _caseBranchBody
            )
            branches

        compileCaseBranchNoBinders :: Tag -> Core.Node -> CaseBranch
        compileCaseBranchNoBinders tag body =
          CaseBranch
            tag
            ( DL.toList $
                DL.cons (mkInstr Pop) $
                  go isTail tempSize refs body
            )

        compileCaseBranch :: Int -> Tag -> Core.Node -> CaseBranch
        compileCaseBranch bindersNum tag body =
          CaseBranch
            tag
            ( DL.toList $
                DL.cons (mkInstr PushTemp) $
                  snocPopTemp isTail $
                    go
                      isTail
                      (tempSize + 1)
                      ( BL.prepend
                          ( map
                              (Ref . ConstrRef . Field tag (TempRef tempSize))
                              (reverse [0 .. bindersNum - 1])
                          )
                          refs
                      )
                      body
            )

        compileCaseDefault :: Core.Node -> Code
        compileCaseDefault =
          DL.toList
            . DL.cons (mkInstr Pop)
            . go isTail tempSize refs

    goIf :: Bool -> Int -> BinderList Value -> Core.If -> Code'
    goIf isTail tempSize refs (Core.If {..}) =
      DL.snoc
        (go False tempSize refs _ifValue)
        ( Branch $
            CmdBranch
              { _cmdBranchInfo = emptyInfo,
                _cmdBranchTrue = DL.toList $ go isTail tempSize refs _ifTrue,
                _cmdBranchFalse = DL.toList $ go isTail tempSize refs _ifFalse
              }
        )

    genOp :: Core.BuiltinOp -> Code'
    genOp = \case
      Core.OpIntAdd -> DL.singleton $ mkBinop IntAdd
      Core.OpIntSub -> DL.singleton $ mkBinop IntSub
      Core.OpIntMul -> DL.singleton $ mkBinop IntMul
      Core.OpIntDiv -> DL.singleton $ mkBinop IntDiv
      Core.OpIntMod -> DL.singleton $ mkBinop IntMod
      Core.OpIntLt -> DL.singleton $ mkBinop IntLt
      Core.OpIntLe -> DL.singleton $ mkBinop IntLe
      Core.OpEq -> DL.singleton $ mkBinop ValEq
      Core.OpShow -> DL.singleton $ mkInstr ValShow
      Core.OpStrConcat -> DL.singleton $ mkBinop StrConcat
      Core.OpStrToInt -> DL.singleton $ mkInstr StrToInt
      Core.OpTrace -> DL.fromList [mkInstr Trace, mkInstr Pop]
      Core.OpFail -> DL.singleton $ mkInstr Failure

    getArgsNum :: Symbol -> Int
    getArgsNum sym =
      fromMaybe
        impossible
        (HashMap.lookup sym (infoTable ^. Core.infoFunctions))
        ^. Core.functionArgsNum

    snocReturn :: Bool -> Code' -> Code'
    snocReturn True code = DL.snoc code (mkInstr Return)
    snocReturn False code = code

    snocPopTemp :: Bool -> Code' -> Code'
    snocPopTemp False code = DL.snoc code (mkInstr PopTemp)
    snocPopTemp True code = code

-- | Be mindful that JuvixAsm types are explicitly uncurried, while
-- Core.Stripped types are always curried. If a function takes `n` arguments,
-- then the first `n` arguments should be uncurried in its JuvixAsm type.
convertType :: Int -> Core.Type -> Type
convertType argsNum ty =
  case ty of
    Core.TyDynamic ->
      TyDynamic
    Core.TyPrim x ->
      convertPrimitiveType x
    Core.TyApp Core.TypeApp {..} ->
      TyInductive (TypeInductive _typeAppSymbol)
    Core.TyFun {} ->
      let (tgt, tyargs) = Core.unfoldType ty
          tyargs' = map convertNestedType tyargs
          tgt' = convertType 0 tgt
       in mkTypeFun (take argsNum tyargs') (mkTypeFun (drop argsNum tyargs') tgt')

convertPrimitiveType :: Core.Primitive -> Type
convertPrimitiveType = \case
  Core.PrimInteger Core.PrimIntegerInfo {..} ->
    TyInteger (TypeInteger _infoMinValue _infoMaxValue)
  Core.PrimBool Core.PrimBoolInfo {..} ->
    TyBool (TypeBool _infoTrueTag _infoFalseTag)
  Core.PrimString ->
    TyString

-- | `convertNestedType` ensures that the conversion of a type with Dynamic in the
-- target is curried. The result of `convertType 0 ty` is always uncurried.
convertNestedType :: Core.Type -> Type
convertNestedType ty =
  case ty of
    Core.TyFun {} ->
      let (tgt, tyargs) = Core.unfoldType ty
       in case tgt of
            Core.TyDynamic ->
              curryType (convertType 0 ty)
            _ ->
              mkTypeFun (map convertNestedType tyargs) (convertType 0 tgt)
    _ ->
      convertType 0 ty

translateInductiveInfo :: Core.InductiveInfo -> InductiveInfo
translateInductiveInfo ii =
  InductiveInfo
    { _inductiveName = ii ^. Core.inductiveName,
      _inductiveLocation = ii ^. Core.inductiveLocation,
      _inductiveSymbol = ii ^. Core.inductiveSymbol,
      _inductiveKind = convertType 0 (ii ^. Core.inductiveKind),
      _inductiveConstructors = ii ^. Core.inductiveConstructors,
      _inductiveRepresentation = IndRepStandard
    }

translateConstructorInfo :: Core.ConstructorInfo -> ConstructorInfo
translateConstructorInfo ci =
  ConstructorInfo
    { _constructorName = ci ^. Core.constructorName,
      _constructorLocation = ci ^. Core.constructorLocation,
      _constructorTag = ci ^. Core.constructorTag,
      _constructorArgsNum = length (typeArgs ty),
      _constructorType = ty,
      _constructorInductive = ci ^. Core.constructorInductive,
      _constructorRepresentation = MemRepConstr
    }
  where
    ty = convertType 0 (ci ^. Core.constructorType)
