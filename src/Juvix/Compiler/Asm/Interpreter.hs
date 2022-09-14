module Juvix.Compiler.Asm.Interpreter where

import Control.Monad
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Interpreter.Extra
import Juvix.Compiler.Asm.Interpreter.Runtime
import Juvix.Compiler.Asm.Language

-- | Interpret JuvixAsm code for a single function. The returned Val is the
-- value on top of the value stack at exit, i.e., when executing a toplevel
-- Return. Throws a runtime error if at exit the value stack has more than one
-- element.
runCode :: InfoTable -> Code -> Val
runCode infoTable = run . evalRuntime . goToplevel
  where
    goToplevel :: Member Runtime r => Code -> Sem r Val
    goToplevel code = do
      goCode code
      popLastValueStack

    goCode :: Member Runtime r => Code -> Sem r ()
    goCode = \case
      cmd : cont -> goCommand cmd cont
      [] -> return ()

    goCommand :: Member Runtime r => Command -> Code -> Sem r ()
    goCommand cmd cont = case cmd of
      Instr (CmdInstr {..}) ->
        goInstr _cmdInstrInstruction cont
      Branch (CmdBranch {..}) -> do
        v <- popValueStack
        case v of
          ValBool True -> goCode _cmdBranchTrue
          ValBool False -> goCode _cmdBranchFalse
          _ -> error "branch on non-boolean"
        goCode cont
      Case (CmdCase {..}) -> do
        v <- topValueStack
        case v of
          ValConstr c -> branch (c ^. constrTag) _cmdCaseBranches _cmdCaseDefault
          _ -> error "case on non-data"
        goCode cont
        where
          branch :: Member Runtime r => Tag -> [CaseBranch] -> Maybe Code -> Sem r ()
          branch tag bs def = case bs of
            (CaseBranch {..}) : _ | _caseBranchTag == tag -> goCode _caseBranchCode
            _ : bs' -> branch tag bs' def
            _ -> goCode $ fromMaybe (error "no matching branch") def

    goInstr :: Member Runtime r => Instruction -> Code -> Sem r ()
    goInstr instr cont = case instr of
      IntAdd ->
        goIntBinOp (\x y -> ValInteger (x + y)) >> goCode cont
      IntSub ->
        goIntBinOp (\x y -> ValInteger (x - y)) >> goCode cont
      IntMul ->
        goIntBinOp (\x y -> ValInteger (x * y)) >> goCode cont
      IntDiv ->
        goIntBinOp (\x y -> ValInteger (x `div` y)) >> goCode cont
      IntLt ->
        goIntBinOp (\x y -> ValBool (x < y)) >> goCode cont
      IntLe ->
        goIntBinOp (\x y -> ValBool (x <= y)) >> goCode cont
      ValEq ->
        goBinOp (\x y -> ValBool (x == y)) >> goCode cont
      Push ref -> do
        v <- getVal ref
        pushValueStack v
        goCode cont
      Pop ->
        popValueStack >> goCode cont
      PushTemp -> do
        v <- popValueStack
        pushTempStack v
        goCode cont
      PopTemp ->
        popTempStack >> goCode cont
      AllocConstr tag -> do
        let ci = getConstrInfo infoTable tag
        args <- replicateM (ci ^. constructorArgsNum) popValueStack
        pushValueStack (ValConstr (Constr tag args))
        goCode cont
      AllocClosure InstrAllocClosure {..} -> do
        unless (_allocClosureArgsNum > 0) $
          error "invalid closure allocation: the number of supplied arguments must be greater than 0"
        args <- replicateM _allocClosureArgsNum popValueStack
        pushValueStack (ValClosure (Closure _allocClosureFunSymbol args))
        goCode cont
      ExtendClosure InstrExtendClosure {..} -> do
        v <- popValueStack
        case v of
          ValClosure cl -> do
            unless (_extendClosureArgsNum > 0) $
              error "invalid closure extension: the number of supplied arguments must be greater than 0"
            extendClosure cl _extendClosureArgsNum
            goCode cont
          _ -> error "invalid closure extension: expected closure on top of value stack"
      Call ic -> do
        (code, frm) <- getCallDetails ic
        pushCallStack cont
        replaceFrame frm
        goCode code
      TailCall ic -> do
        unless (null cont) (error "invalid tail call")
        (code, frm) <- getCallDetails ic
        replaceFrame frm
        goCode code
      CallClosures (InstrCallClosures {..}) ->
        callClosures False _callClosuresArgsNum cont
      TailCallClosures (InstrCallClosures {..}) ->
        callClosures True _callClosuresArgsNum cont
      Return -> do
        unless (null cont) (error "invalid return")
        isToplevel <- fmap not hasCaller
        if
            | isToplevel -> return ()
            | otherwise -> do
                v <- popLastValueStack
                cont' <- popCallStack
                replaceFrame (cont' ^. contFrame)
                pushValueStack v
                goCode (cont' ^. contCode)

    goBinOp :: Member Runtime r => (Val -> Val -> Val) -> Sem r ()
    goBinOp op = do
      v1 <- popValueStack
      v2 <- popValueStack
      pushValueStack (op v1 v2)

    goIntBinOp :: Member Runtime r => (Integer -> Integer -> Val) -> Sem r ()
    goIntBinOp op = goBinOp $ \v1 v2 ->
      case (v1, v2) of
        (ValInteger i1, ValInteger i2) -> op i1 i2
        _ -> error "invalid operation: expected two integers on value stack"

    getVal :: Member Runtime r => Value -> Sem r Val
    getVal = \case
      ConstInt i -> return (ValInteger i)
      ConstBool b -> return (ValBool b)
      ConstString s -> return (ValString s)
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

    getCallDetails :: Member Runtime r => InstrCall -> Sem r (Code, Frame)
    getCallDetails InstrCall {..} = case _callType of
      CallFun sym -> do
        let fi = getFunInfo infoTable sym
        when
          (_callArgsNum /= fi ^. functionArgsNum)
          (error "invalid indirect call: supplied arguments number not equal to expected arguments number")
        args <- replicateM (fi ^. functionArgsNum) popValueStack
        return (fi ^. functionCode, frameFromFunctionInfo fi args)
      CallClosure -> do
        v <- popValueStack
        case v of
          ValClosure cl -> do
            let fi = getFunInfo infoTable (cl ^. closureSymbol)
            let n = length (cl ^. closureArgs)
            when
              (n >= fi ^. functionArgsNum)
              (error "invalid closure: too many arguments")
            when
              (_callArgsNum /= fi ^. functionArgsNum - n)
              (error "invalid indirect call: supplied arguments number not equal to expected arguments number")
            frm <- getCallFrame cl fi _callArgsNum
            return (fi ^. functionCode, frm)
          _ -> error "invalid indirect call: expected closure on top of value stack"

    getCallFrame :: Member Runtime r => Closure -> FunctionInfo -> Int -> Sem r Frame
    getCallFrame cl fi argsNum = do
      args <- replicateM argsNum popValueStack
      return $ frameFromFunctionInfo fi ((cl ^. closureArgs) ++ args)

    extendClosure :: Member Runtime r => Closure -> Int -> Sem r ()
    extendClosure cl n = do
      args <- replicateM n popValueStack
      pushValueStack
        ( ValClosure
            ( Closure
                (cl ^. closureSymbol)
                (cl ^. closureArgs ++ args)
            )
        )

    callClosures :: Member Runtime r => Bool -> Int -> Code -> Sem r ()
    callClosures isTail argsNum cont = do
      v <- popValueStack
      case v of
        ValClosure cl -> do
          let fi = getFunInfo infoTable (cl ^. closureSymbol)
          let n = fi ^. functionArgsNum - length (cl ^. closureArgs)
          when
            (n < 0)
            (error "invalid closure: too many arguments")
          if
              | n > argsNum -> do
                  extendClosure cl argsNum
                  if
                      | isTail -> goInstr Return cont
                      | otherwise -> goCode cont
              | n == argsNum -> do
                  frm <- getCallFrame cl fi n
                  unless isTail $ do
                    unless (null cont) (error "invalid tail call")
                    pushCallStack cont
                  replaceFrame frm
                  goCode (fi ^. functionCode)
              | otherwise -> do
                  let instr = mkInstr ((if isTail then TailCallClosures else CallClosures) (InstrCallClosures (argsNum - n)))
                  frm <- getCallFrame cl fi n
                  pushCallStack (instr : cont)
                  replaceFrame frm
                  goCode (fi ^. functionCode)
        _ -> error "invalid indirect call: expected closure on top of value stack"
