module Juvix.Compiler.Asm.Translation.FromCore where

import Data.DList qualified as DL
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Core.Data.BinderList qualified as BL
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Core.Extra qualified as Core
import Juvix.Compiler.Core.Language qualified as Core
import Juvix.Compiler.Core.Language.Info qualified as Info
import Juvix.Compiler.Core.Language.Info.ArgsNumInfo

type BinderList = BL.BinderList

-- DList for O(1) snoc and append
type Code' = DL.DList Instruction

-- Generate code for a single function.
--
-- Assumptions:
-- - lambda-lifted, i.e., lambdas occur only at the top,
-- - well-typed (no illegal applications),
-- - no evaluation-only nodes,
-- - no axioms,
-- - fully applied constructors and builtins,
-- - ArgsNumInfo available for each Var node.
genCode :: Core.InfoTable -> Core.Node -> Code
genCode infoTable = DL.toList . goToplevel
  where
    goToplevel :: Core.Node -> Code'
    goToplevel node =
      let (k, body) = Core.unfoldLambdas node
       in go True 0 (BL.fromList $ reverse (map (Ref . ArgRef) [0 .. k - 1])) body

    -- Assumption: the BinderList does not contain references to the value stack
    -- (directly or indirectly).
    go :: Bool -> Int -> BinderList Value -> Core.Node -> Code'
    go isTail tempSize refs node = case node of
      Core.Var {..} ->
        snocReturn isTail $ DL.singleton (Push (BL.lookup varIndex refs))
      Core.Ident {..} ->
        snocReturn isTail $ DL.singleton (AllocClosure identSymbol 0)
      Core.Constant _ (Core.ConstInteger i) ->
        snocReturn isTail $ DL.singleton (Push (ConstInt i))
      Core.Constant _ (Core.ConstBool b) ->
        snocReturn isTail $ DL.singleton (Push (ConstBool b))
      Core.App {} ->
        let (fun, args) = Core.unfoldApp node
         in case fun of
              Core.Ident {..} ->
                if
                    | argsNum > length args ->
                        snocReturn isTail $
                          DL.snoc
                            (DL.concat (map (go False tempSize refs) args))
                            (AllocClosure identSymbol (length args))
                    | argsNum == length args ->
                        DL.snoc
                          (DL.concat (map (go False tempSize refs) args))
                          ((if isTail then TailCall else Call) (CallFun identSymbol))
                    | otherwise -> impossible
                where
                  argsNum =
                    fromMaybe
                      impossible
                      (HashMap.lookup identSymbol (infoTable ^. Core.infoIdents))
                      ^. Core.identArgsNum
              Core.Var {..} ->
                if
                    | argsNum > length args ->
                        snocReturn isTail $
                          DL.snoc
                            ( DL.snoc
                                (DL.concat (map (go False tempSize refs) args))
                                (Push (BL.lookup varIndex refs))
                            )
                            (ExtendClosure (length args))
                    | argsNum == length args ->
                        DL.snoc
                          ( DL.snoc
                              (DL.concat (map (go False tempSize refs) args))
                              (Push (BL.lookup varIndex refs))
                          )
                          ((if isTail then TailCall else Call) CallClosure)
                    | otherwise -> impossible
                where
                  argsNum =
                    fromMaybe impossible (Info.lookup kArgsNumInfo varInfo)
                      ^. infoArgsNum
              _ -> impossible
      Core.BuiltinApp {..} ->
        snocReturn isTail $
          DL.snoc
            (DL.concat (map (go False tempSize refs) builtinArgs))
            (genOp builtinOp)
      Core.ConstrApp {..} ->
        snocReturn isTail $
          DL.snoc
            (DL.concat (map (go False tempSize refs) constrArgs))
            (AllocConstr constrTag)
      Core.Let {..} ->
        DL.append
          (DL.snoc (DL.snoc (go False tempSize refs letValue) (Store tempSize)) Pop)
          (go isTail (tempSize + 1) (BL.extend (Ref (TempRef tempSize)) refs) letBody)
      Core.Case {..} ->
        DL.snoc
          (DL.snoc (go False tempSize refs caseValue) (Store tempSize))
          ( Case
              ( map
                  ( \(Core.CaseBranch {..}) ->
                      CaseBranch
                        caseTag
                        ( DL.toList $
                            go
                              isTail
                              (tempSize + 1)
                              ( BL.prepend
                                  ( map
                                      (Ref . ConstrRef . Field (TempRef tempSize))
                                      (reverse [0 .. caseBindersNum - 1])
                                  )
                                  refs
                              )
                              caseBranch
                        )
                  )
                  caseBranches
              )
              (fmap (DL.toList . go isTail (tempSize + 1) refs) caseDefault)
          )
      Core.If {..} ->
        DL.snoc
          (go False tempSize refs ifValue)
          ( Branch
              (DL.toList $ go isTail tempSize refs ifTrueBranch)
              (DL.toList $ go isTail tempSize refs ifFalseBranch)
          )
      _ -> impossible

    genOp :: Core.BuiltinOp -> Instruction
    genOp = \case
      Core.OpIntAdd -> IntAdd
      Core.OpIntSub -> IntSub
      Core.OpIntMul -> IntMul
      Core.OpIntDiv -> IntDiv
      Core.OpIntEq -> IntEq
      Core.OpIntLt -> IntLt
      Core.OpIntLe -> IntLe

    snocReturn :: Bool -> Code' -> Code'
    snocReturn True code = DL.snoc code Return
    snocReturn False code = code
