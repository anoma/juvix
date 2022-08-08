module Juvix.Compiler.Asm.Interpreter where

import Control.Monad
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Interpreter.Extra
import Juvix.Compiler.Asm.Interpreter.Runtime
import Juvix.Compiler.Asm.Language

-- The returned Val is the value on top of the value stack at exit, i.e., when
-- executing a toplevel Return. Throws a runtime error if at exit the value
-- stack has more than one element.
runCode :: InfoTable -> Code -> Val
runCode infoTable = run . evalRuntime . goToplevel
  where
    goToplevel :: Member Runtime r => Code -> Sem r Val
    goToplevel code = do
      goCode code
      popLastValueStack

    goCode :: Member Runtime r => Code -> Sem r ()
    goCode = \case
      instr : cont -> goInstr instr cont
      [] -> return ()

    goInstr :: Member Runtime r => Instruction -> Code -> Sem r ()
    goInstr instr cont = case instr of
      IntAdd -> goIntBinOp (\x y -> ValInteger (x + y)) >> goCode cont
      IntSub -> goIntBinOp (\x y -> ValInteger (x - y)) >> goCode cont
      IntMul -> goIntBinOp (\x y -> ValInteger (x * y)) >> goCode cont
      IntDiv -> goIntBinOp (\x y -> ValInteger (x `div` y)) >> goCode cont
      IntEq -> goIntBinOp (\x y -> ValBool (x == y)) >> goCode cont
      IntLt -> goIntBinOp (\x y -> ValBool (x < y)) >> goCode cont
      IntLe -> goIntBinOp (\x y -> ValBool (x <= y)) >> goCode cont
      Push ref -> do
        v <- getVal ref
        pushValueStack v
        goCode cont
      Pop -> void popValueStack
      Store off -> do
        v <- topValueStack
        writeTemp off v
        goCode cont
      AllocConstr tag -> do
        let ci = getConstrInfo tag
        args <- replicateM (ci ^. constrInfoArgsNum) popValueStack
        pushValueStack (ValConstr (Constr tag (reverse args)))
        goCode cont
      AllocClosure {..} -> do
        args <- replicateM allocClosureArgsNum popValueStack
        pushValueStack (ValClosure (Closure allocClosureFunSymbol (reverse args)))
        goCode cont
      ExtendClosure {..} -> do
        v <- popValueStack
        case v of
          ValClosure cl -> do
            args <- replicateM extendClosureArgsNum popValueStack
            pushValueStack
              ( ValClosure
                  ( Closure
                      (cl ^. closureSymbol)
                      (cl ^. closureArgs ++ reverse args)
                  )
              )
            goCode cont
          _ -> error "invalid closure extension: expected closure on top of value stack"
      Branch {..} -> do
        v <- popValueStack
        case v of
          ValBool True -> goCode branchTrue
          ValBool False -> goCode branchFalse
          _ -> error "branch on non-boolean"
        goCode cont
      Case {..} -> do
        v <- popValueStack
        case v of
          ValConstr c -> branch (c ^. constrTag) caseBranches caseDefault
          _ -> error "case on non-data"
        goCode cont
        where
          branch :: Member Runtime r => Tag -> [CaseBranch] -> Maybe Code -> Sem r ()
          branch tag bs def = case bs of
            (CaseBranch {..}) : _ | _caseBranchTag == tag -> goCode _caseBranchCode
            _ : bs' -> branch tag bs' def
            _ -> goCode $ fromMaybe (error "no matching branch") def
      Call callType -> do
        (code, frm) <- getCallDetails callType
        pushCallStack cont
        replaceFrame frm
        goCode code
      TailCall callType -> do
        unless (null cont) (error "invalid tail call")
        (code, frm) <- getCallDetails callType
        replaceFrame frm
        goCode code
      Return -> do
        isToplevel <- fmap not hasCaller
        if
            | isToplevel -> return ()
            | otherwise -> do
                v <- popLastValueStack
                cont' <- popCallStack
                replaceFrame (cont' ^. contFrame)
                pushValueStack v
                goCode (cont' ^. contCode)

    goIntBinOp :: Member Runtime r => (Integer -> Integer -> Val) -> Sem r ()
    goIntBinOp op = do
      v2 <- popValueStack
      v1 <- popValueStack
      case (v1, v2) of
        (ValInteger i1, ValInteger i2) -> pushValueStack (op i1 i2)
        _ -> error "invalid operation: expected two integers on value stack"

    getVal :: Member Runtime r => Value -> Sem r Val
    getVal = \case
      ConstInt i -> return (ValInteger i)
      ConstBool b -> return (ValBool b)
      Ref r -> getMemVal r

    getMemVal :: Member Runtime r => MemValue -> Sem r Val
    getMemVal = \case
      StackRef -> topValueStack
      ArgRef off -> readArg off
      TempRef off -> readTemp off
      ConstrRef cr -> do
        v <- getMemVal (cr ^. fieldValue)
        case v of
          ValConstr ctr ->
            if
                | cr ^. fieldOffset < length (ctr ^. constrArgs) ->
                    return $ (ctr ^. constrArgs) !! (cr ^. fieldOffset)
                | otherwise ->
                    error "invalid constructor field access"
          _ -> error "invalid memory access: expected a constructor"

    popLastValueStack :: Member Runtime r => Sem r Val
    popLastValueStack = do
      v <- popValueStack
      isNull <- nullValueStack
      unless
        isNull
        (error "value stack not empty on function return")
      return v

    getCallDetails :: Member Runtime r => CallType -> Sem r (Code, Frame)
    getCallDetails = \case
      CallFun sym -> do
        let fi = getFunInfo sym
        args <- replicateM (fi ^. functionInfoArgsNum) popValueStack
        return (fi ^. functionInfoCode, frameFromFunctionInfo fi (reverse args))
      CallClosure -> do
        v <- popValueStack
        case v of
          ValClosure cl -> do
            let fi = getFunInfo (cl ^. closureSymbol)
            let n = length (cl ^. closureArgs)
            when
              (n >= fi ^. functionInfoArgsNum)
              (error "invalid closure: too many arguments")
            args' <- replicateM (fi ^. functionInfoArgsNum - n) popValueStack
            return
              ( fi ^. functionInfoCode,
                frameFromFunctionInfo fi ((cl ^. closureArgs) ++ reverse args')
              )
          _ -> error "invalid indirect call: expected closure on top of value stack"

    getFunInfo :: Symbol -> FunctionInfo
    getFunInfo sym = fromMaybe (error "invalid function symbol") (HashMap.lookup sym (infoTable ^. infoFunctions))

    getConstrInfo :: Tag -> ConstrInfo
    getConstrInfo tag = fromMaybe (error "invalid constructor tag") (HashMap.lookup tag (infoTable ^. infoConstrs))
