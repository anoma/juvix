module Juvix.Compiler.Asm.Translation.FromCore where

import Data.DList qualified as DL
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.Stripped.InfoTable qualified as Core
import Juvix.Compiler.Core.Language.Stripped qualified as Core

type BinderList = BL.BinderList

-- DList for O(1) snoc and append
type Code' = DL.DList Instruction

-- Generate code for a single function.
genCode :: Core.InfoTable -> Core.FunctionInfo -> FunctionInfo
genCode infoTable fi =
  let code =
        DL.toList $
          go
            True
            0
            ( BL.fromList $
                reverse (map (Ref . ArgRef) [0 .. fi ^. Core.functionArgsNum - 1])
            )
            (fi ^. Core.functionBody)
   in FunctionInfo
        { _functionName = fi ^. Core.functionName,
          _functionSymbol = fi ^. Core.functionSymbol,
          _functionArgsNum = fi ^. Core.functionArgsNum,
          _functionType = fi ^. Core.functionType,
          _functionCode = code
        }
  where
    unimplemented :: forall a. a
    unimplemented = error "not yet implemented"

    -- Assumption: the BinderList does not contain references to the value stack
    -- (directly or indirectly).
    go :: Bool -> Int -> BinderList Value -> Core.Node -> Code'
    go isTail tempSize refs node = case node of
      Core.NVar (Core.Var {..}) ->
        snocReturn isTail $ DL.singleton (Push (BL.lookup _varIndex refs))
      Core.NIdt (Core.Ident {..}) ->
        if
            | getArgsNum _identSymbol == 0 ->
                DL.singleton ((if isTail then TailCall else Call) (CallFun _identSymbol))
            | otherwise ->
                snocReturn isTail $ DL.singleton (AllocClosure _identSymbol 0)
      Core.NCst (Core.Constant _ (Core.ConstInteger i)) ->
        snocReturn isTail $ DL.singleton (Push (ConstInt i))
      Core.NCtr (Core.Constr _ (Core.BuiltinTag Core.TagTrue) _) ->
        snocReturn isTail $ DL.singleton (Push (ConstBool True))
      Core.NCtr (Core.Constr _ (Core.BuiltinTag Core.TagFalse) _) ->
        snocReturn isTail $ DL.singleton (Push (ConstBool False))
      Core.NApp (Core.Apps {..}) ->
        case _appsFun of
          Core.FunIdent (Core.Ident {..}) ->
            if
                | argsNum > length _appsArgs ->
                    snocReturn isTail $
                      DL.snoc
                        (DL.concat (map (go False tempSize refs) _appsArgs))
                        (AllocClosure _identSymbol (length _appsArgs))
                | argsNum == length _appsArgs ->
                    DL.snoc
                      (DL.concat (map (go False tempSize refs) _appsArgs))
                      ((if isTail then TailCall else Call) (CallFun _identSymbol))
                | otherwise -> unimplemented
            where
              argsNum = getArgsNum _identSymbol
          Core.FunVar (Core.Var {..}) ->
            if
                | argsNum > length _appsArgs ->
                    snocReturn isTail $
                      DL.snoc
                        ( DL.snoc
                            (DL.concat (map (go False tempSize refs) _appsArgs))
                            (Push (BL.lookup _varIndex refs))
                        )
                        (ExtendClosure (length _appsArgs))
                | argsNum == length _appsArgs ->
                    DL.snoc
                      ( DL.snoc
                          (DL.concat (map (go False tempSize refs) _appsArgs))
                          (Push (BL.lookup _varIndex refs))
                      )
                      ((if isTail then TailCall else Call) CallClosure)
                | otherwise -> unimplemented
            where
              argsNum :: Int
              argsNum = unimplemented
      Core.NBlt (Core.BuiltinApp {..}) ->
        snocReturn isTail $
          DL.snoc
            (DL.concat (map (go False tempSize refs) _builtinAppArgs))
            (genOp _builtinAppOp)
      Core.NCtr (Core.Constr {..}) ->
        snocReturn isTail $
          DL.snoc
            (DL.concat (map (go False tempSize refs) _constrArgs))
            (AllocConstr _constrTag)
      Core.NLet (Core.Let {..}) ->
        DL.append
          (DL.snoc (go False tempSize refs _letValue) PushTemp)
          (go isTail (tempSize + 1) (BL.extend (Ref (TempRef tempSize)) refs) _letBody)
      Core.NCase (Core.Case {..}) ->
        -- TODO: special case for if-then-else
        DL.snoc
          (go False tempSize refs _caseValue)
          ( Case
              ( map
                  ( \(Core.CaseBranch {..}) ->
                      if
                          | _caseBranchBindersNum == 0 ->
                              CaseBranch
                                _caseBranchTag
                                ( DL.toList $
                                    DL.cons Pop $
                                      go isTail tempSize refs _caseBranchBody
                                )
                          | otherwise ->
                              CaseBranch
                                _caseBranchTag
                                ( DL.toList $
                                    DL.cons PushTemp $
                                      go
                                        isTail
                                        (tempSize + 1)
                                        ( BL.prepend
                                            ( map
                                                (Ref . ConstrRef . Field (TempRef tempSize))
                                                (reverse [0 .. _caseBranchBindersNum - 1])
                                            )
                                            refs
                                        )
                                        _caseBranchBody
                                )
                  )
                  _caseBranches
              )
              (fmap (DL.toList . DL.cons Pop . go isTail (tempSize + 1) refs) _caseDefault)
          )
      _ -> unimplemented

    genOp :: Core.BuiltinOp -> Instruction
    genOp = \case
      Core.OpIntAdd -> IntAdd
      Core.OpIntSub -> IntSub
      Core.OpIntMul -> IntMul
      Core.OpIntDiv -> IntDiv
      Core.OpIntLt -> IntLt
      Core.OpIntLe -> IntLe
      Core.OpEq -> ValEq
      _ -> unimplemented

    getArgsNum :: Symbol -> Int
    getArgsNum sym =
      fromMaybe
        impossible
        (HashMap.lookup sym (infoTable ^. Core.infoFunctions))
        ^. Core.functionArgsNum

    snocReturn :: Bool -> Code' -> Code'
    snocReturn True code = DL.snoc code Return
    snocReturn False code = code
