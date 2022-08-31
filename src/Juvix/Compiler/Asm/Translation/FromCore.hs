module Juvix.Compiler.Asm.Translation.FromCore where

import Data.DList qualified as DL
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Extra qualified as Core
import Juvix.Compiler.Core.Language qualified as Core

type BinderList = BL.BinderList

-- DList for O(1) snoc and append
type Code' = DL.DList Instruction

-- Generate code for a single function.
--
-- Assumptions:
-- - lambda-lifted, i.e., lambdas occur only at the top,
-- - eta-expanded at the top,
-- - well-typed (no illegal applications),
-- - no compilation-only nodes (Pi, Univ, TypeConstr),
-- - no evaluation-only nodes (Closure),
-- - fully applied constructors and builtins,
-- - squashed constructor and builtin applications.
genCode :: Core.InfoTable -> Core.Node -> Code
genCode infoTable = DL.toList . goToplevel
  where
    unimplemented :: forall a. a
    unimplemented = error "not yet implemented"

    goToplevel :: Core.Node -> Code'
    goToplevel node =
      let (k, body) = Core.unfoldLambdas node
       in go True 0 (BL.fromList $ reverse (map (Ref . ArgRef) [0 .. k - 1])) body

    -- Assumption: the BinderList does not contain references to the value stack
    -- (directly or indirectly).
    go :: Bool -> Int -> BinderList Value -> Core.Node -> Code'
    go isTail tempSize refs node = case node of
      Core.Var {..} ->
        snocReturn isTail $ DL.singleton (Push (BL.lookup _varIndex refs))
      Core.Ident {..} ->
        if
            | getArgsNum _identSymbol == 0 ->
                DL.singleton ((if isTail then TailCall else Call) (CallFun _identSymbol))
            | otherwise ->
                snocReturn isTail $ DL.singleton (AllocClosure _identSymbol 0)
      Core.Constant _ (Core.ConstInteger i) ->
        snocReturn isTail $ DL.singleton (Push (ConstInt i))
      Core.Constr _ (Core.BuiltinTag Core.TagTrue) _ ->
        snocReturn isTail $ DL.singleton (Push (ConstBool True))
      Core.Constr _ (Core.BuiltinTag Core.TagFalse) _ ->
        snocReturn isTail $ DL.singleton (Push (ConstBool False))
      Core.App {} ->
        let (fun, args) = Core.unfoldApp node
         in case fun of
              Core.Ident {..} ->
                if
                    | argsNum > length args ->
                        snocReturn isTail $
                          DL.snoc
                            (DL.concat (map (go False tempSize refs) args))
                            (AllocClosure _identSymbol (length args))
                    | argsNum == length args ->
                        DL.snoc
                          (DL.concat (map (go False tempSize refs) args))
                          ((if isTail then TailCall else Call) (CallFun _identSymbol))
                    | otherwise -> unimplemented
                where
                  argsNum = getArgsNum _identSymbol
              Core.Var {..} ->
                if
                    | argsNum > length args ->
                        snocReturn isTail $
                          DL.snoc
                            ( DL.snoc
                                (DL.concat (map (go False tempSize refs) args))
                                (Push (BL.lookup _varIndex refs))
                            )
                            (ExtendClosure (length args))
                    | argsNum == length args ->
                        DL.snoc
                          ( DL.snoc
                              (DL.concat (map (go False tempSize refs) args))
                              (Push (BL.lookup _varIndex refs))
                          )
                          ((if isTail then TailCall else Call) CallClosure)
                    | otherwise -> unimplemented
                where
                  argsNum :: Int
                  argsNum = unimplemented
              _ -> impossible
      Core.BuiltinApp {..} ->
        snocReturn isTail $
          DL.snoc
            (DL.concat (map (go False tempSize refs) _builtinArgs))
            (genOp _builtinOp)
      Core.Constr {..} ->
        snocReturn isTail $
          DL.snoc
            (DL.concat (map (go False tempSize refs) _constrArgs))
            (AllocConstr _constrTag)
      Core.Let {..} ->
        DL.append
          (DL.snoc (go False tempSize refs _letValue) PushTemp)
          (go isTail (tempSize + 1) (BL.extend (Ref (TempRef tempSize)) refs) _letBody)
      Core.Case {..} ->
        -- TODO: special case for if-then-else
        DL.snoc
          (go False tempSize refs _caseValue)
          ( Case
              ( map
                  ( \(Core.CaseBranch {..}) ->
                      if
                          | _caseBindersNum == 0 ->
                              CaseBranch
                                _caseTag
                                ( DL.toList $
                                    DL.cons Pop $
                                      go isTail tempSize refs _caseBranch
                                )
                          | otherwise ->
                              CaseBranch
                                _caseTag
                                ( DL.toList $
                                    DL.cons PushTemp $
                                      go
                                        isTail
                                        (tempSize + 1)
                                        ( BL.prepend
                                            ( map
                                                (Ref . ConstrRef . Field (TempRef tempSize))
                                                (reverse [0 .. _caseBindersNum - 1])
                                            )
                                            refs
                                        )
                                        _caseBranch
                                )
                  )
                  _caseBranches
              )
              (fmap (DL.toList . DL.cons Pop . go isTail (tempSize + 1) refs) _caseDefault)
          )
      _ -> impossible

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
        (HashMap.lookup sym (infoTable ^. Core.infoIdents))
        ^. Core.identArgsNum

    snocReturn :: Bool -> Code' -> Code'
    snocReturn True code = DL.snoc code Return
    snocReturn False code = code
