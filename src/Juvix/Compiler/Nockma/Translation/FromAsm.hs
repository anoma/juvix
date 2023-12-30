module Juvix.Compiler.Nockma.Translation.FromAsm where

import Juvix.Compiler.Asm.Data.InfoTable qualified as Asm
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Stdlib
import Juvix.Prelude hiding (Atom, Path)

type FunctionId = Symbol

type FunctionPaths = HashMap FunctionId Path

type Offset = Natural

data CompilerFunction = CompilerFunction
  { _compilerFunctionName :: FunctionId,
    _compilerFunction :: Sem '[Compiler, Reader FunctionPaths] ()
  }

data StackId
  = CurrentFunction
  | ValueStack
  | CallStack
  | TempStack
  | FunctionsLibrary
  | StandardLibrary
  deriving stock (Enum, Bounded, Eq, Show)

data StdlibFunction
  = StdlibDec
  | StdlibAdd
  | StdlibSub
  | StdlibMul
  | StdlibDiv
  | StdlibMod
  | StdlibLt
  | StdlibLe

stdlibNumArgs :: StdlibFunction -> Natural
stdlibNumArgs = \case
  StdlibDec -> 1
  StdlibAdd -> 2
  StdlibSub -> 2
  StdlibMul -> 2
  StdlibMod -> 2
  StdlibDiv -> 2
  StdlibLe -> 2
  StdlibLt -> 2

stdlibPath :: StdlibFunction -> Path
stdlibPath =
  decodePath' . EncodedPath . \case
    StdlibDec -> 342
    StdlibAdd -> 20
    StdlibSub -> 47
    StdlibMul -> 4
    StdlibDiv -> 170
    StdlibMod -> 46
    StdlibLe -> 84
    StdlibLt -> 343

numStacks :: (Integral a) => a
numStacks = fromIntegral (length (allElements @StackId))

data Compiler m a where
  Verbatim :: Term Natural -> Compiler m ()
  PushOnto :: StackId -> Term Natural -> Compiler m ()
  Crash :: Compiler m ()
  PopFrom :: StackId -> Compiler m ()
  TestEqOn :: StackId -> Compiler m ()
  CallHelper :: Bool -> FunctionId -> Natural -> Compiler m ()
  IncrementOn :: StackId -> Compiler m ()
  Branch :: m () -> m () -> Compiler m ()
  CallStdlib :: StdlibFunction -> Compiler m ()
  AsmReturn :: Compiler m ()

stackPath :: StackId -> Path
stackPath s = indexStack (fromIntegral (fromEnum s))

indexStack :: Natural -> Path
indexStack idx = replicate idx R ++ [L]

indexInPath :: Path -> Natural -> Path
indexInPath p idx = p ++ indexStack idx

topOfStack :: StackId -> Path
topOfStack s = indexInStack s 0

indexInStack :: StackId -> Natural -> Path
indexInStack s idx = stackPath s ++ indexStack idx

pathToArgumentsArea :: Path
pathToArgumentsArea = topOfStack CurrentFunction ++ [R]

pathToArg :: Natural -> Path
pathToArg = indexInPath pathToArgumentsArea

-- | Construct a path rooted at he head of a named stack
pathInStack :: StackId -> Path -> Path
pathInStack s p = stackPath s ++ p

makeSem ''Compiler
makeLenses ''CompilerFunction

fromAsm :: Asm.Symbol -> Asm.InfoTable -> ([Term Natural], Term Natural)
fromAsm mainSym Asm.InfoTable {..} =
  let funs = map compileFunction allFunctions
   in runCompilerWith funs mainFun
  where
    mainFun :: CompilerFunction
    mainFun =
      CompilerFunction
        { _compilerFunctionName = mainSym,
          _compilerFunction = compile mainCode
        }

    mainCode :: Asm.Code
    mainCode = _infoFunctions ^?! at mainSym . _Just . Asm.functionCode

    allFunctions :: [Asm.FunctionInfo]
    allFunctions = filter notMain (toList _infoFunctions)
      where
        notMain :: Asm.FunctionInfo -> Bool
        notMain Asm.FunctionInfo {..} = _functionSymbol /= mainSym

    compileFunction :: Asm.FunctionInfo -> CompilerFunction
    compileFunction Asm.FunctionInfo {..} =
      CompilerFunction
        { _compilerFunctionName = _functionSymbol,
          _compilerFunction = compile _functionCode
        }

compile :: forall r. (Members '[Compiler] r) => Asm.Code -> Sem r ()
compile = mapM_ goCommand
  where
    goCommand :: Asm.Command -> Sem r ()
    goCommand = \case
      Asm.Instr i -> goCmdInstr i
      Asm.Branch b -> goBranch b
      Asm.Case c -> goCase c

    goCase :: Asm.CmdCase -> Sem r ()
    goCase = error "TODO"

    goBranch :: Asm.CmdBranch -> Sem r ()
    goBranch Asm.CmdBranch {..} = branch (compile _cmdBranchTrue) (compile _cmdBranchFalse)

    goBinop :: Asm.Opcode -> Sem r ()
    goBinop o = case o of
      Asm.IntAdd -> callStdlib StdlibAdd
      Asm.IntSub -> callStdlib StdlibSub
      Asm.IntMul -> callStdlib StdlibMul
      Asm.IntDiv -> callStdlib StdlibDiv
      Asm.IntMod -> callStdlib StdlibMod
      Asm.IntLt -> callStdlib StdlibLt
      Asm.IntLe -> callStdlib StdlibLe
      Asm.ValEq -> testEqOn ValueStack
      Asm.StrConcat -> stringsErr

    goPush :: Asm.Value -> Sem r ()
    goPush = \case
      Asm.ConstInt i
        | i < 0 -> unsupported "negative numbers"
        | otherwise -> pushNat (fromInteger i)
      Asm.ConstBool i -> push (nockBoolLiteral i)
      Asm.ConstString {} -> stringsErr
      Asm.ConstUnit -> error "TODO"
      Asm.ConstVoid -> error "TODO"
      Asm.Ref r -> pushMemValue r
      where
        pushMemValue :: Asm.MemValue -> Sem r ()
        pushMemValue = \case
          Asm.DRef r -> pushDirectRef r
          Asm.ConstrRef r -> goConstrRef r

        goConstrRef :: Asm.Field -> Sem r ()
        goConstrRef = error "TODO"

        pushDirectRef :: Asm.DirectRef -> Sem r ()
        pushDirectRef = \case
          Asm.StackRef -> push (OpAddress # topOfStack ValueStack)
          Asm.ArgRef a -> pushArgRef (fromIntegral a)
          Asm.TempRef off -> push (OpAddress # indexInStack TempStack (fromIntegral off))

    goPrealloc :: Asm.InstrPrealloc -> Sem r ()
    goPrealloc = error "TODO"

    goAllocConstr :: Asm.Tag -> Sem r ()
    goAllocConstr = error "TODO"

    goAllocClosure :: Asm.InstrAllocClosure -> Sem r ()
    goAllocClosure = error "TODO"

    goExtendClosure :: Asm.InstrExtendClosure -> Sem r ()
    goExtendClosure = error "TODO"

    goCallHelper :: Bool -> Asm.InstrCall -> Sem r ()
    goCallHelper isTail Asm.InstrCall {..} = case _callType of
      Asm.CallFun fun -> callHelper isTail fun (fromIntegral _callArgsNum)
      Asm.CallClosure -> error "TODO"

    goCall :: Asm.InstrCall -> Sem r ()
    goCall = goCallHelper False

    goTailCall :: Asm.InstrCall -> Sem r ()
    goTailCall = goCallHelper True

    goCmdInstr :: Asm.CmdInstr -> Sem r ()
    goCmdInstr Asm.CmdInstr {..} = case _cmdInstrInstruction of
      Asm.Binop op -> goBinop op
      Asm.Push p -> goPush p
      Asm.Pop -> pop
      Asm.PopTemp -> popFrom TempStack
      Asm.PushTemp -> pushTemp
      Asm.Failure -> crash
      Asm.Prealloc i -> goPrealloc i
      Asm.AllocConstr i -> goAllocConstr i
      Asm.AllocClosure c -> goAllocClosure c
      Asm.ExtendClosure c -> goExtendClosure c
      Asm.Call c -> goCall c
      Asm.TailCall c -> goTailCall c
      Asm.Return -> asmReturn
      Asm.ValShow -> stringsErr
      Asm.StrToInt -> stringsErr
      Asm.Trace -> unsupported "trace"
      Asm.Dump -> unsupported "dump"
      Asm.CallClosures {} -> impossible
      Asm.TailCallClosures {} -> impossible

unsupported :: Text -> a
unsupported thing = error ("The Nockma backend does not support" <> thing)

stringsErr :: a
stringsErr = unsupported "strings"

seqTerms :: [Term Natural] -> Term Natural
seqTerms = foldl' step (OpAddress # emptyPath) . reverse
  where
    step :: Term Natural -> Term Natural -> Term Natural
    step acc t = OpSequence # t # acc

makeList :: [Term Natural] -> Term Natural
makeList ts = foldr1 (#) (ts ++ [TermAtom nockNil])

remakeList :: (Foldable l) => l (Term Natural) -> Term Natural
remakeList ts = foldr1 (#) (toList ts ++ [OpQuote # nockNil'])

nockNil' :: Term Natural
nockNil' = TermAtom nockNil

initStack :: [Term Natural] -> Term Natural
initStack defs = makeList (initSubStack <$> allElements)
  where
    initSubStack :: StackId -> Term Natural
    initSubStack = \case
      CurrentFunction -> nockNil'
      ValueStack -> nockNil'
      CallStack -> nockNil'
      TempStack -> nockNil'
      StandardLibrary -> stdlib
      FunctionsLibrary -> makeList defs

push :: (Members '[Compiler] r) => Term Natural -> Sem r ()
push = pushOnto ValueStack

execCompiler :: (Member (Reader FunctionPaths) r) => Sem (Compiler ': r) a -> Sem r (Term Natural)
execCompiler = fmap fst . runCompiler

runCompiler :: (Member (Reader FunctionPaths) r) => Sem (Compiler ': r) a -> Sem r (Term Natural, a)
runCompiler sem = do
  (ts, a) <- runOutputList (re sem)
  return (seqTerms ts, a)

runCompilerWith :: [CompilerFunction] -> CompilerFunction -> ([Term Natural], Term Natural)
runCompilerWith libFuns mainFun =
  let entryCommand :: (Members '[Compiler] r) => Sem r ()
      entryCommand = call (mainFun ^. compilerFunctionName) 0
      entryTerm =
        seqTerms
          . run
          . runReader functionLocations
          . execOutputList
          . re
          $ entryCommand
      compiledFuns :: [Term Natural]
      compiledFuns =
        (# nockNil')
          <$> ( run
                  . runReader functionLocations
                  . mapM (execCompiler . (^. compilerFunction))
                  $ allfuns
              )
   in (compiledFuns, entryTerm)
  where
    allfuns = mainFun : libFuns
    functionLocations :: FunctionPaths
    functionLocations =
      hashMap
        [ (_compilerFunctionName, indexInStack FunctionsLibrary i)
          | (i, CompilerFunction {..}) <- zip [0 ..] allfuns
        ]

callEnum :: (Enum funId, Members '[Compiler] r) => funId -> Natural -> Sem r ()
callEnum f = call (fromIntegral (fromEnum f))

call :: (Members '[Compiler] r) => FunctionId -> Natural -> Sem r ()
call = callHelper False

tcall :: (Members '[Compiler] r) => FunctionId -> Natural -> Sem r ()
tcall = callHelper True

callHelper' :: (Members '[Output (Term Natural), Reader FunctionPaths] r) => Bool -> FunctionId -> Natural -> Sem r ()
callHelper' isTail funName funArgsNum = do
  -- 1. Obtain the path to the function
  funPath <- fromJust <$> asks @FunctionPaths (^. at funName)

  -- 2.
  --   i. Take a copy of the value stack without the arguments to the function
  --   ii. Push this copy to the call stack
  let storeOnStack =
        if
            | isTail -> replaceOnStack
            | otherwise -> pushOnStack
  output (storeOnStack CallStack (stackPop ValueStack funArgsNum))

  -- 3.
  --  i. Take a copy of the function from the function library
  --  ii. Replace its argument area with the arguments from the value stack
  --  iii. Push this copy to the current function stack

  -- Setup function to call with its arguments
  let funWithArgs = OpReplace # ([R] # stackTake ValueStack funArgsNum) # OpAddress # funPath

  -- Push it to the current function stack
  output (storeOnStack CurrentFunction funWithArgs)

  -- 4. Replace the value stack with nil
  output (resetStack ValueStack)

  -- 5. Evaluate the function in the context of the whole nock stack
  -- 6. See documentation for asmReturn'
  output (OpCall # ((topOfStack CurrentFunction ++ [L]) # (OpAddress # emptyPath)))

asmReturn' :: (Members '[Output (Term Natural), Reader FunctionPaths] r) => Sem r ()
asmReturn' = do
  -- Restore the previous value stack (created in call'.2.). i.e copy the previous value stack
  -- from the call stack and push the result (the head of the current value stack) to it.
  output (replaceStack ValueStack ((OpAddress # topOfStack ValueStack) # (OpAddress # topOfStack CallStack)))

  -- discard the 'activation' frame
  output (popStack CallStack)
  output (popStack CurrentFunction)

testEq :: (Members '[Compiler] r) => Sem r ()
testEq = testEqOn ValueStack

testEqOn' :: (Members '[Output (Term Natural)] r) => StackId -> Sem r ()
testEqOn' s = output (replaceOnStackN 2 s (OpEq # stackSliceAsCell s 0 1))

incrementOn' :: (Members '[Output (Term Natural)] r) => StackId -> Sem r ()
incrementOn' s = output (replaceOnStack s (OpInc # stackSliceAsCell s 0 0))

callStdlib' :: (Members '[Output (Term Natural)] r) => StdlibFunction -> Sem r ()
callStdlib' f = do
  let fNumArgs = stdlibNumArgs f
      fPath = stdlibPath f
      decodeFn = OpCall # (fPath # (OpAddress # stackPath StandardLibrary))
      arguments = OpSequence # (OpAddress # [R]) # stdlibStackTake ValueStack fNumArgs
      extractResult = (OpAddress # [L]) # (OpAddress # [R, R])
      callFn = OpPush # (OpCall # [L] # (OpReplace # ([R, L] # arguments) # (OpAddress # [L]))) # extractResult

  output (OpPush # decodeFn # callFn)
  output (replaceTopStackN fNumArgs ValueStack)
  where
    stdlibStackTake :: StackId -> Natural -> Term Natural
    stdlibStackTake sn n = foldr1 (#) (take (fromIntegral n) [OpAddress # indexInStack sn i | i <- [0 ..]])

branch' ::
  (Functor f, Members '[Output (Term Natural), Reader FunctionPaths] r) =>
  m () ->
  m () ->
  Sem (WithTactics Compiler f m r) (f ())
branch' t f = do
  termT <- runT t >>= raise . execCompiler . (pop >>)
  termF <- runT f >>= raise . execCompiler . (pop >>)
  (output >=> pureT) (OpIf # (OpAddress # pathInStack ValueStack [L]) # termT # termF)

pushArgRef :: (Members '[Compiler] r) => Offset -> Sem r ()
pushArgRef n = push (OpAddress # pathToArg n)

pushTemp :: (Members '[Compiler] r) => Sem r ()
pushTemp = do
  pushOnto TempStack (OpAddress # topOfStack ValueStack)
  pop

re :: (Member (Reader FunctionPaths) r) => Sem (Compiler ': r) a -> Sem (Output (Term Natural) ': r) a
re = reinterpretH $ \case
  PushOnto s n -> outputT (pushOnStack s n)
  PopFrom s -> outputT (popStack s)
  Verbatim s -> outputT s
  CallHelper isTail funName funArgsNum -> callHelper' isTail funName funArgsNum >>= pureT
  IncrementOn s -> incrementOn' s >>= pureT
  Branch t f -> branch' t f
  CallStdlib f -> callStdlib' f >>= pureT
  AsmReturn -> asmReturn' >>= pureT
  TestEqOn s -> testEqOn' s >>= pureT
  Crash -> outputT (OpAddress # OpAddress # OpAddress)
  where
    outputT :: (Functor f, Member (Output (Term Natural)) r) => Term Natural -> Sem (WithTactics e f m r) (f ())
    outputT = pureT <=< output

add :: (Members '[Compiler] r) => Sem r ()
add = callStdlib StdlibAdd

dec :: (Members '[Compiler] r) => Sem r ()
dec = callStdlib StdlibDec

increment :: (Members '[Compiler] r) => Sem r ()
increment = incrementOn ValueStack

pop :: (Members '[Compiler] r) => Sem r ()
pop = popFrom ValueStack

stackPop :: StackId -> Natural -> Term Natural
stackPop s n = OpAddress # pathInStack s (replicate n R)

stackTake :: StackId -> Natural -> Term Natural
stackTake sn n = remakeList (take (fromIntegral n) [OpAddress # indexInStack sn i | i <- [0 ..]])

stackSliceHelper :: StackId -> Natural -> Natural -> NonEmpty (Term Natural)
stackSliceHelper sn fromIx toIx = fromMaybe err (nonEmpty [OpAddress # indexInStack sn i | i <- indices])
  where
    err :: a
    err = error "impossible: empty slice"
    indices
      | fromIx <= toIx = [fromIx .. toIx]
      | otherwise = impossible

stackSliceAsCell :: StackId -> Natural -> Natural -> Term Natural
stackSliceAsCell sn a b = foldr1 (#) (stackSliceHelper sn a b)

-- | Takes a slice of a stack. Both indices are inclusive
stackSliceAsList :: StackId -> Natural -> Natural -> Term Natural
stackSliceAsList sn fromIx toIx = remakeList (stackSliceHelper sn fromIx toIx)

pushOnStack :: StackId -> Term Natural -> Term Natural
pushOnStack s t = OpPush # t # topStack s

replaceOnStackN :: Natural -> StackId -> Term Natural -> Term Natural
replaceOnStackN numToReplace s t = OpPush # t # replaceTopStackN numToReplace s

replaceOnStack :: StackId -> Term Natural -> Term Natural
replaceOnStack = replaceOnStackN 1

popStack :: StackId -> Term Natural
popStack = popStackN 1

popStackN :: Natural -> StackId -> Term Natural
popStackN n sn =
  remakeList
    [ let p = stackPath s
          a
            | sn == s = p ++ replicate n R
            | otherwise = p
       in OpAddress # a
      | s <- allElements
    ]

replaceStack :: StackId -> Term Natural -> Term Natural
replaceStack sn t =
  remakeList
    [ if
          | sn == s -> t
          | otherwise -> OpAddress # (stackPath s)
      | s <- allElements
    ]

resetStack :: StackId -> Term Natural
resetStack sn = replaceStack sn (OpQuote # nockNil')

-- | Reconstruct the value-stack / call-stack cell by moving the global head to the
-- respective stack head.

--- [h [s1 s1 s3 nil]]
--- [ s1 .. [h si] ... sn nil]
topStack :: StackId -> Term Natural
topStack sn =
  remakeList
    [ let p = OpAddress # (R : stackPath s)
       in if
              | sn == s -> (OpAddress # [L]) # p
              | otherwise -> p
      | s <- allElements
    ]

replaceTopStackN :: Natural -> StackId -> Term Natural
replaceTopStackN n sn =
  remakeList
    [ let p = R : stackPath s
       in if
              | sn == s -> (OpAddress # [L]) # (OpAddress # p ++ replicate n R)
              | otherwise -> OpAddress # p
      | s <- allElements
    ]

replaceTopStack :: StackId -> Term Natural
replaceTopStack = replaceTopStackN 1

pushNat :: (Member Compiler r) => Natural -> Sem r ()
pushNat = pushNatOnto ValueStack

pushNatOnto :: (Member Compiler r) => StackId -> Natural -> Sem r ()
pushNatOnto s n = pushOnto s (OpQuote # toNock n)

compileAndRunNock :: [CompilerFunction] -> CompilerFunction -> Term Natural
compileAndRunNock funs mainfun =
  let (functionTerms, t) = runCompilerWith funs mainfun
   in evalCompiledNock functionTerms t

evalCompiledNock :: [Term Natural] -> Term Natural -> Term Natural
evalCompiledNock functionTerms mainTerm =
  let stack = initStack functionTerms
      evalT =
        run
          . runError @(ErrNockNatural Natural)
          . runError @NockEvalError
          $ eval stack mainTerm
   in case evalT of
        Left e -> error (show e)
        Right ev -> case ev of
          Left e -> error (show e)
          Right res -> res

-- | Used in testing and app
getStack :: StackId -> Term Natural -> Term Natural
getStack st m = fromRight' (run (runError @NockEvalError (subTerm m (stackPath st))))
