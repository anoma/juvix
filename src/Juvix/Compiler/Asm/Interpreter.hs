module Juvix.Compiler.Asm.Interpreter
  ( module Juvix.Compiler.Asm.Interpreter,
    module Juvix.Compiler.Asm.Interpreter.Base,
  )
where

import Control.Exception qualified as Exception
import Control.Monad
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Extra.Base
import Juvix.Compiler.Asm.Interpreter.Base
import Juvix.Compiler.Asm.Interpreter.Extra
import Juvix.Compiler.Asm.Interpreter.Runtime
import Juvix.Compiler.Asm.Pretty

-- | Interpret JuvixAsm code for a single function. The returned Val is the
-- value on top of the value stack at exit, i.e., when executing a toplevel
-- Return. Throws a runtime runtimeError if at exit the value stack has more than one
-- element.
runCode :: InfoTable -> FunctionInfo -> IO Val
runCode = hRunCode stdout

hRunCode :: Handle -> InfoTable -> FunctionInfo -> IO Val
hRunCode h infoTable = runM . hEvalRuntime h infoTable . runCodeR infoTable

runCodeR :: Member Runtime r => InfoTable -> FunctionInfo -> Sem r Val
runCodeR infoTable funInfo = goCode (funInfo ^. functionCode) >> popLastValueStack
  where
    goCode :: Member Runtime r => Code -> Sem r ()
    goCode = \case
      cmd : cont -> goCommand cmd cont
      [] -> return ()

    goCommand :: Member Runtime r => Command -> Code -> Sem r ()
    goCommand cmd cont = case cmd of
      Instr CmdInstr {..} -> do
        registerLocation (_cmdInstrInfo ^. commandInfoLocation)
        goInstr (_cmdInstrInfo ^. commandInfoLocation) _cmdInstrInstruction cont
      Branch CmdBranch {..} -> do
        registerLocation (_cmdBranchInfo ^. commandInfoLocation)
        v <- popValueStack
        case v of
          ValBool True -> goCode _cmdBranchTrue
          ValBool False -> goCode _cmdBranchFalse
          _ -> runtimeError "branch on non-boolean"
        goCode cont
      Case CmdCase {..} -> do
        registerLocation (_cmdCaseInfo ^. commandInfoLocation)
        v <- topValueStack
        case v of
          ValConstr c -> branch (c ^. constrTag) _cmdCaseBranches _cmdCaseDefault
          _ -> runtimeError "case on non-data"
        goCode cont
        where
          branch :: Member Runtime r => Tag -> [CaseBranch] -> Maybe Code -> Sem r ()
          branch tag bs def = case bs of
            (CaseBranch {..}) : _ | _caseBranchTag == tag -> goCode _caseBranchCode
            _ : bs' -> branch tag bs' def
            _ -> case def of
              Just x -> goCode x
              Nothing -> runtimeError "no matching branch"

    goInstr :: Member Runtime r => Maybe Location -> Instruction -> Code -> Sem r ()
    goInstr loc instr cont = case instr of
      Binop IntAdd ->
        goIntBinOp (\x y -> ValInteger (x + y)) >> goCode cont
      Binop IntSub ->
        goIntBinOp (\x y -> ValInteger (x - y)) >> goCode cont
      Binop IntMul ->
        goIntBinOp (\x y -> ValInteger (x * y)) >> goCode cont
      Binop IntDiv -> do
        goIntBinOp'
          ( \x y ->
              if
                  | y == 0 -> runtimeError "division by zero"
                  | otherwise -> return $ ValInteger (x `quot` y)
          )
        goCode cont
      Binop IntMod -> do
        goIntBinOp'
          ( \x y ->
              if
                  | y == 0 -> runtimeError "division by zero"
                  | otherwise -> return $ ValInteger (x `rem` y)
          )
        goCode cont
      Binop IntLt ->
        goIntBinOp (\x y -> ValBool (x < y)) >> goCode cont
      Binop IntLe ->
        goIntBinOp (\x y -> ValBool (x <= y)) >> goCode cont
      Binop ValEq ->
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
      Trace -> do
        v <- topValueStack
        logMessage (printVal v)
        goCode cont
      Dump -> do
        dumpState
        goCode cont
      Failure -> do
        v <- topValueStack
        runtimeError $ mappend "failure: " (printVal v)
      Prealloc {} ->
        goCode cont
      AllocConstr tag -> do
        let ci = getConstrInfo infoTable tag
        args <- replicateM (ci ^. constructorArgsNum) popValueStack
        pushValueStack (ValConstr (Constr tag args))
        goCode cont
      AllocClosure InstrAllocClosure {..} -> do
        args <- replicateM _allocClosureArgsNum popValueStack
        pushValueStack (ValClosure (Closure _allocClosureFunSymbol args))
        goCode cont
      ExtendClosure InstrExtendClosure {..} -> do
        v <- popValueStack
        case v of
          ValClosure cl -> do
            unless (_extendClosureArgsNum > 0) $
              runtimeError "invalid closure extension: the number of supplied arguments must be greater than 0"
            extendClosure cl _extendClosureArgsNum
            goCode cont
          _ -> runtimeError "invalid closure extension: expected closure on top of value stack"
      Call ic -> do
        (code, frm) <- getCallDetails loc ic
        pushCallStack cont
        replaceFrame frm
        goCode code
      TailCall ic -> do
        unless (null cont) (runtimeError "invalid tail call")
        (code, frm) <- getCallDetails loc ic
        replaceTailFrame frm
        goCode code
      CallClosures (InstrCallClosures {..}) ->
        callClosures loc False _callClosuresArgsNum cont
      TailCallClosures (InstrCallClosures {..}) ->
        callClosures loc True _callClosuresArgsNum cont
      Return -> do
        unless (null cont) (runtimeError "invalid return")
        isToplevel <- fmap not hasCaller
        if
            | isToplevel -> return ()
            | otherwise -> do
                v <- popLastValueStack
                cont' <- popCallStack
                replaceFrame (cont' ^. contFrame)
                pushValueStack v
                goCode (cont' ^. contCode)

    goBinOp' :: Member Runtime r => (Val -> Val -> Sem r Val) -> Sem r ()
    goBinOp' op = do
      v1 <- popValueStack
      v2 <- popValueStack
      v <- op v1 v2
      pushValueStack v

    goBinOp :: Member Runtime r => (Val -> Val -> Val) -> Sem r ()
    goBinOp op = goBinOp' (\x y -> return (op x y))

    goIntBinOp' :: Member Runtime r => (Integer -> Integer -> Sem r Val) -> Sem r ()
    goIntBinOp' op = goBinOp' $ \v1 v2 ->
      case (v1, v2) of
        (ValInteger i1, ValInteger i2) -> op i1 i2
        _ -> runtimeError "invalid operation: expected two integers on value stack"

    goIntBinOp :: Member Runtime r => (Integer -> Integer -> Val) -> Sem r ()
    goIntBinOp op = goIntBinOp' (\v1 v2 -> return (op v1 v2))

    getVal :: Member Runtime r => Value -> Sem r Val
    getVal = \case
      ConstInt i -> return (ValInteger i)
      ConstBool b -> return (ValBool b)
      ConstString s -> return (ValString s)
      ConstUnit -> return ValUnit
      ConstVoid -> return ValVoid
      Ref r -> getMemVal r

    getMemVal :: Member Runtime r => MemValue -> Sem r Val
    getMemVal = \case
      DRef dr -> getDirectRef dr
      ConstrRef cr -> do
        v <- getDirectRef (cr ^. fieldRef)
        case v of
          ValConstr ctr ->
            if
                | cr ^. fieldOffset < length (ctr ^. constrArgs) ->
                    return $ (ctr ^. constrArgs) !! (cr ^. fieldOffset)
                | otherwise ->
                    runtimeError "invalid constructor field access"
          _ -> runtimeError "invalid memory access: expected a constructor"

    getDirectRef :: Member Runtime r => DirectRef -> Sem r Val
    getDirectRef = \case
      StackRef -> topValueStack
      ArgRef off -> readArg off
      TempRef off -> readTemp off

    popLastValueStack :: Member Runtime r => Sem r Val
    popLastValueStack = do
      v <- popValueStack
      isNull <- nullValueStack
      unless
        isNull
        (runtimeError "value stack not empty on function return")
      return v

    getCallDetails :: Member Runtime r => Maybe Location -> InstrCall -> Sem r (Code, Frame)
    getCallDetails loc InstrCall {..} = case _callType of
      CallFun sym -> do
        let fi = getFunInfo infoTable sym
        when
          (_callArgsNum /= fi ^. functionArgsNum)
          (runtimeError "invalid direct call: supplied arguments number not equal to expected arguments number")
        args <- replicateM (fi ^. functionArgsNum) popValueStack
        return (fi ^. functionCode, frameFromFunctionInfo loc fi args)
      CallClosure -> do
        v <- popValueStack
        case v of
          ValClosure cl -> do
            let fi = getFunInfo infoTable (cl ^. closureSymbol)
                n = length (cl ^. closureArgs)
            when
              (n >= fi ^. functionArgsNum)
              (runtimeError "invalid closure: too many arguments")
            when
              (_callArgsNum /= fi ^. functionArgsNum - n)
              (runtimeError "invalid indirect call: supplied arguments number not equal to expected arguments number")
            frm <- getCallFrame loc cl fi _callArgsNum
            return (fi ^. functionCode, frm)
          _ -> runtimeError "invalid indirect call: expected closure on top of value stack"

    getCallFrame :: Member Runtime r => Maybe Location -> Closure -> FunctionInfo -> Int -> Sem r Frame
    getCallFrame loc cl fi argsNum = do
      args <- replicateM argsNum popValueStack
      return $ frameFromFunctionInfo loc fi ((cl ^. closureArgs) ++ args)

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

    callClosures :: Member Runtime r => Maybe Location -> Bool -> Int -> Code -> Sem r ()
    callClosures loc isTail argsNum cont = do
      v <- popValueStack
      case v of
        ValClosure cl -> do
          let fi = getFunInfo infoTable (cl ^. closureSymbol)
          let n = fi ^. functionArgsNum - length (cl ^. closureArgs)
          when
            (n < 0)
            (runtimeError "invalid closure: too many arguments")
          if
              | n > argsNum -> do
                  extendClosure cl argsNum
                  if
                      | isTail -> goInstr loc Return cont
                      | otherwise -> goCode cont
              | n == argsNum -> do
                  frm <- getCallFrame loc cl fi n
                  if
                      | isTail -> do
                          unless (null cont) $
                            runtimeError "invalid tail call"
                          replaceTailFrame frm
                      | otherwise -> do
                          pushCallStack cont
                          replaceFrame frm
                  goCode (fi ^. functionCode)
              | otherwise -> do
                  let instr = mkInstr ((if isTail then TailCallClosures else CallClosures) (InstrCallClosures (argsNum - n)))
                  frm <- getCallFrame loc cl fi n
                  pushCallStack (instr : cont)
                  replaceFrame frm
                  goCode (fi ^. functionCode)
        _ -> runtimeError "invalid indirect call: expected closure on top of value stack"

    printVal :: Val -> Text
    printVal = \case
      ValString s -> s
      v -> ppPrint infoTable v

-- | Interpret JuvixAsm code and the resulting IO actions.
runCodeIO :: InfoTable -> FunctionInfo -> IO Val
runCodeIO = hRunCodeIO stdin stdout

hRunCodeIO :: Handle -> Handle -> InfoTable -> FunctionInfo -> IO Val
hRunCodeIO hin hout infoTable funInfo = do
  v <- hRunCode hout infoTable funInfo
  hRunIO hin hout infoTable funInfo v

-- | Interpret IO actions.
hRunIO :: Handle -> Handle -> InfoTable -> FunctionInfo -> Val -> IO Val
hRunIO hin hout infoTable funInfo = \case
  ValConstr (Constr (BuiltinTag TagReturn) [x]) -> return x
  ValConstr (Constr (BuiltinTag TagBind) [x, f]) -> do
    x' <- hRunIO hin hout infoTable funInfo x
    let code = [Instr (CmdInstr (CommandInfo Nothing) (Call (InstrCall CallClosure 1)))]
    let r =
          pushValueStack x'
            >> pushValueStack f
            >> runCodeR infoTable funInfo {_functionCode = code}
    x'' <- runM (hEvalRuntime hout infoTable r)
    hRunIO hin hout infoTable funInfo x''
  ValConstr (Constr (BuiltinTag TagWrite) [ValString s]) -> do
    hPutStr hout s
    return ValVoid
  ValConstr (Constr (BuiltinTag TagWrite) [arg]) -> do
    hPutStr hout (ppPrint infoTable arg)
    return ValVoid
  ValConstr (Constr (BuiltinTag TagReadLn) []) -> do
    hFlush hout
    s <- hGetLine hin
    return (ValString s)
  val ->
    return val

catchRunErrorIO :: IO a -> IO (Either AsmError a)
catchRunErrorIO ma =
  Exception.catch
    (Exception.evaluate ma >>= \ma' -> ma' <&> Right)
    (\(ex :: RunError) -> return (Left (toAsmError ex)))

toAsmError :: RunError -> AsmError
toAsmError (RunError {..}) =
  AsmError
    { _asmErrorMsg =
        "runtime error: "
          <> _runErrorMsg
          <> "\n\nStacktrace\n----------\n\n"
          <> ppTrace (_runErrorState ^. runtimeInfoTable) _runErrorState,
      _asmErrorLoc = _runErrorState ^. runtimeLocation
    }
