module Juvix.Compiler.Asm.Translation.FromCore where

import Data.DList qualified as DL
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Language.Type
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.Stripped.InfoTable qualified as Core
import Juvix.Compiler.Core.Language.Stripped qualified as Core

type BinderList = BL.BinderList

-- DList for O(1) snoc and append
type Code' = DL.DList Command

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
        { _functionName = maybe "function" (^. nameText) (fi ^. Core.functionName),
          _functionLocation = fmap (^. nameLoc) (fi ^. Core.functionName),
          _functionSymbol = fi ^. Core.functionSymbol,
          _functionArgsNum = fi ^. Core.functionArgsNum,
          _functionType = convertType (fi ^. Core.functionType),
          _functionCode = code
        }
  where
    unimplemented :: forall a. a
    unimplemented = error "not yet implemented"

    -- Assumption: the BinderList does not contain references to the value stack
    -- (directly or indirectly).
    go :: Bool -> Int -> BinderList Value -> Core.Node -> Code'
    go isTail tempSize refs node = case node of
      Core.NVar v@Core.Var {} -> goVar isTail refs v
      Core.NIdt idt@Core.Ident {} -> goIdent isTail idt
      Core.NCst cst@Core.Constant {} -> goConstant isTail cst
      Core.NApp apps@Core.Apps {} -> goApps isTail tempSize refs apps
      Core.NBlt blt@Core.BuiltinApp {} -> goBuiltinApp isTail tempSize refs blt
      Core.NCtr ctr@Core.Constr {} -> goConstr isTail tempSize refs ctr
      Core.NLet lt@Core.Let {} -> goLet isTail tempSize refs lt
      Core.NCase c@Core.Case {} -> goCase isTail tempSize refs c

    goVar :: Bool -> BinderList Value -> Core.Var -> Code'
    goVar isTail refs (Core.Var {..}) =
      snocReturn isTail $
        DL.singleton $
          mkInstr $
            Push (BL.lookup _varIndex refs)

    goIdent :: Bool -> Core.Ident -> Code'
    goIdent isTail (Core.Ident {..}) =
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
      _ -> unimplemented

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
                      unimplemented
              where
                argsNum = getArgsNum _identSymbol
            Core.FunVar (Core.Var {..}) ->
              if
                  | argsNum > suppliedArgsNum ->
                      snocReturn isTail $
                        DL.snoc
                          ( DL.snoc
                              (DL.concat (map (go False tempSize refs) suppliedArgs))
                              (mkInstr $ Push (BL.lookup _varIndex refs))
                          )
                          (mkInstr $ ExtendClosure (InstrExtendClosure suppliedArgsNum))
                  | argsNum == suppliedArgsNum ->
                      DL.snoc
                        ( DL.snoc
                            (DL.concat (map (go False tempSize refs) suppliedArgs))
                            (mkInstr $ Push (BL.lookup _varIndex refs))
                        )
                        (mkInstr $ (if isTail then TailCall else Call) (InstrCall CallClosure argsNum))
                  | otherwise ->
                      -- Here use CallClosures or TailCallClosures.
                      unimplemented
              where
                -- If the number of arguments is not available (the target of the
                -- variable's type is dynamic), then we should use CallClosures or
                -- TailCallClosures.
                argsNum :: Int
                argsNum = unimplemented

    goBuiltinApp :: Bool -> Int -> BinderList Value -> Core.BuiltinApp -> Code'
    goBuiltinApp isTail tempSize refs (Core.BuiltinApp {..}) =
      snocReturn isTail $
        DL.snoc
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
        (DL.snoc (go False tempSize refs _letValue) (mkInstr PushTemp))
        (snocPopTemp isTail $ go isTail (tempSize + 1) (BL.extend (Ref (DRef (TempRef tempSize))) refs) _letBody)

    goCase :: Bool -> Int -> BinderList Value -> Core.Case -> Code'
    goCase isTail tempSize refs (Core.Case {..}) =
      -- TODO: special case for if-then-else (use Branch instead of Case)
      DL.snoc
        (go False tempSize refs _caseValue)
        ( Case $
            CmdCase
              { _cmdCaseInfo = emptyInfo,
                _cmdCaseInductive = 0, -- TODO!
                _cmdCaseBranches =
                  map
                    ( \(Core.CaseBranch {..}) ->
                        if
                            | _caseBranchBindersNum == 0 ->
                                CaseBranch
                                  _caseBranchTag
                                  ( DL.toList $
                                      DL.cons (mkInstr Pop) $
                                        go isTail tempSize refs _caseBranchBody
                                  )
                            | otherwise ->
                                CaseBranch
                                  _caseBranchTag
                                  ( DL.toList $
                                      DL.cons (mkInstr PushTemp) $
                                        snocPopTemp isTail $
                                          go
                                            isTail
                                            (tempSize + 1)
                                            ( BL.prepend
                                                ( map
                                                    (Ref . ConstrRef . Field _caseBranchTag (TempRef tempSize))
                                                    (reverse [0 .. _caseBranchBindersNum - 1])
                                                )
                                                refs
                                            )
                                            _caseBranchBody
                                  )
                    )
                    _caseBranches,
                _cmdCaseDefault =
                  fmap
                    ( DL.toList
                        . DL.cons (mkInstr Pop)
                        . go isTail tempSize refs
                    )
                    _caseDefault
              }
        )

    genOp :: Core.BuiltinOp -> Command
    genOp = \case
      Core.OpIntAdd -> mkInstr IntAdd
      Core.OpIntSub -> mkInstr IntSub
      Core.OpIntMul -> mkInstr IntMul
      Core.OpIntDiv -> mkInstr IntDiv
      Core.OpIntLt -> mkInstr IntLt
      Core.OpIntLe -> mkInstr IntLe
      Core.OpEq -> mkInstr ValEq
      _ -> unimplemented

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

    convertType :: Core.Type -> Type
    convertType = unimplemented
