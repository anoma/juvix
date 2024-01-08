module Juvix.Compiler.Nockma.Translation.FromAsm where

import Juvix.Compiler.Asm.Data.InfoTable qualified as Asm
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Stdlib
import Juvix.Prelude hiding (Atom, Path)

type UserFunctionId = Symbol

data FunctionId
  = UserFunction UserFunctionId
  | BuiltinFunction BuiltinFunctionId
  deriving stock (Generic, Eq)

instance Hashable FunctionId

data BuiltinFunctionId
  = BuiltinPow2Go
  | BuiltinPow2
  | BuiltinAppendRights
  deriving stock (Eq, Enum, Bounded, Generic)

instance Hashable BuiltinFunctionId

data FunctionInfo = FunctionInfo
  { _functionPath :: Path,
    _functionArity :: Natural
  }

data CompilerCtx = CompilerCtx
  { _compilerFunctionInfos :: HashMap FunctionId FunctionInfo,
    _compilerConstructorArities :: ConstructorArities
  }

type ConstructorArities = HashMap Asm.Tag Natural

type Offset = Natural

data CompilerFunction = CompilerFunction
  { _compilerFunctionName :: FunctionId,
    _compilerFunctionArity :: Natural,
    _compilerFunction :: Sem '[Compiler, Reader CompilerCtx] ()
  }

data StackId
  = CurrentFunction
  | ValueStack
  | TempStack
  | CallStack
  | StandardLibrary
  | FunctionsLibrary
  deriving stock (Enum, Bounded, Eq, Show)

-- | A closure has the following structure:
-- [code totalArgsNum argsNum args], where
-- 1. code is code to run when fully applied.
-- 2. totalArgsNum is the number of arguments that the function
--     which created the closure expects.
-- 3. argsNum is the number of arguments that have been applied to the closure.
-- 4. args is the list of args that have been applied.
--    The length of the list should be argsNum.
data ClosurePathId
  = ClosureCode
  | ClosureTotalArgsNum
  | ClosureArgsNum
  | ClosureArgs
  deriving stock (Bounded, Enum)

pathFromEnum :: (Enum a) => a -> Path
pathFromEnum = indexStack . fromIntegral . fromEnum

closurePath :: ClosurePathId -> Path
closurePath = pathFromEnum

data ConstructorPathId
  = ConstructorTag
  | ConstructorArgs
  deriving stock (Bounded, Enum)

constructorPath :: ConstructorPathId -> Path
constructorPath = pathFromEnum

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
  PopNAndPushOnto :: StackId -> Natural -> Term Natural -> Compiler m ()
  PopFromN :: Natural -> StackId -> Compiler m ()
  TestEqOn :: StackId -> Compiler m ()
  CallHelper :: Bool -> Maybe FunctionId -> Natural -> Compiler m ()
  IncrementOn :: StackId -> Compiler m ()
  Branch :: m () -> m () -> Compiler m ()
  Save :: Bool -> m () -> Compiler m ()
  CallStdlibOn :: StackId -> StdlibFunction -> Compiler m ()
  AsmReturn :: Compiler m ()
  GetConstructorArity :: Asm.Tag -> Compiler m Natural
  GetFunctionArity :: FunctionId -> Compiler m Natural
  GetFunctionPath :: FunctionId -> Compiler m Path

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
makeLenses ''CompilerCtx
makeLenses ''FunctionInfo

termFromParts :: (Bounded p, Enum p) => (p -> Term Natural) -> Term Natural
termFromParts f = remakeList [f pi | pi <- allElements]

makeClosure :: (ClosurePathId -> Term Natural) -> Term Natural
makeClosure = termFromParts

makeConstructor :: (ConstructorPathId -> Term Natural) -> Term Natural
makeConstructor = termFromParts

foldTerms :: NonEmpty (Term Natural) -> Term Natural
foldTerms = foldr1 (#)

fromAsm :: Asm.Symbol -> Asm.InfoTable -> (NonEmpty (Term Natural), Term Natural)
fromAsm mainSym Asm.InfoTable {..} =
  let funs = map compileFunction allFunctions
      constrs :: ConstructorArities
      constrs = fromIntegral . (^. Asm.constructorArgsNum) <$> _infoConstrs
   in runCompilerWith constrs funs mainFun
  where
    mainFun :: CompilerFunction
    mainFun =
      CompilerFunction
        { _compilerFunctionName = UserFunction mainSym,
          _compilerFunctionArity = 0,
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
        { _compilerFunctionName = UserFunction _functionSymbol,
          _compilerFunctionArity = fromIntegral _functionArgsNum,
          _compilerFunction = compile _functionCode
        }

fromOffsetRef :: Asm.OffsetRef -> Natural
fromOffsetRef = fromIntegral . (^. Asm.offsetRefOffset)

-- | Generic constructors are encoded as [tag args], where args is a
-- nil terminated list.
goConstructor :: Asm.Tag -> [Term Natural] -> Term Natural
goConstructor t args = case t of
  Asm.BuiltinTag b -> goBuiltinTag b
  Asm.UserTag _moduleId num ->
    makeConstructor $ \case
      ConstructorTag -> OpQuote # (fromIntegral num :: Natural)
      ConstructorArgs -> remakeList args
  where
    goBuiltinTag :: Asm.BuiltinDataTag -> Term Natural
    goBuiltinTag = \case
      Asm.TagTrue -> nockBoolLiteral True
      Asm.TagFalse -> nockBoolLiteral False
      Asm.TagReturn -> impossible
      Asm.TagBind -> impossible
      Asm.TagWrite -> impossible
      Asm.TagReadLn -> impossible

compile :: forall r. (Members '[Compiler] r) => Asm.Code -> Sem r ()
compile = mapM_ goCommand
  where
    goCommand :: Asm.Command -> Sem r ()
    goCommand = \case
      Asm.Instr i -> goCmdInstr i
      Asm.Branch b -> goBranch b
      Asm.Case c -> goCase c
      Asm.Save s -> goSave s

    goSave :: Asm.CmdSave -> Sem r ()
    goSave cmd = save (cmd ^. Asm.cmdSaveIsTail) (compile (cmd ^. Asm.cmdSaveCode))

    goCase :: Asm.CmdCase -> Sem r ()
    goCase c = do
      let def = compile <$> c ^. Asm.cmdCaseDefault
          branches =
            [ (b ^. Asm.caseBranchTag, compile (b ^. Asm.caseBranchCode))
              | b <- c ^. Asm.cmdCaseBranches
            ]
      caseCmd def branches

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
      Asm.ValEq -> testEq
      Asm.StrConcat -> stringsErr

    goPush :: Asm.Value -> Sem r ()
    goPush = \case
      Asm.ConstInt i
        | i < 0 -> unsupported "negative numbers"
        | otherwise -> pushNat (fromInteger i)
      Asm.ConstBool i -> push (nockBoolLiteral i)
      Asm.ConstString {} -> stringsErr
      Asm.ConstUnit -> push constUnit
      Asm.ConstVoid -> push constVoid
      Asm.Ref r -> pushMemValue r
      where
        pushMemValue :: Asm.MemValue -> Sem r ()
        pushMemValue = \case
          Asm.DRef r -> pushDirectRef r
          Asm.ConstrRef r ->
            pushConstructorField
              (r ^. Asm.fieldRef)
              (fromIntegral (r ^. Asm.fieldOffset))

    goAllocClosure :: Asm.InstrAllocClosure -> Sem r ()
    goAllocClosure a = allocClosure (UserFunction (a ^. Asm.allocClosureFunSymbol)) (fromIntegral (a ^. Asm.allocClosureArgsNum))

    goExtendClosure :: Asm.InstrExtendClosure -> Sem r ()
    goExtendClosure a = extendClosure (fromIntegral (a ^. Asm.extendClosureArgsNum))

    goCallHelper :: Bool -> Asm.InstrCall -> Sem r ()
    goCallHelper isTail Asm.InstrCall {..} =
      let funName = case _callType of
            Asm.CallFun fun -> Just fun
            Asm.CallClosure -> Nothing
       in callHelper isTail (UserFunction <$> funName) (fromIntegral _callArgsNum)

    goCall :: Asm.InstrCall -> Sem r ()
    goCall = goCallHelper False

    goTailCall :: Asm.InstrCall -> Sem r ()
    goTailCall = goCallHelper True

    goCmdInstr :: Asm.CmdInstr -> Sem r ()
    goCmdInstr Asm.CmdInstr {..} = case _cmdInstrInstruction of
      Asm.Binop op -> goBinop op
      Asm.Push p -> goPush p
      Asm.Pop -> pop
      Asm.Failure -> crash
      Asm.AllocConstr i -> allocConstr i
      Asm.AllocClosure c -> goAllocClosure c
      Asm.ExtendClosure c -> goExtendClosure c
      Asm.Call c -> goCall c
      Asm.TailCall c -> goTailCall c
      Asm.Return -> asmReturn
      Asm.ArgsNum -> closureArgsNum
      Asm.ValShow -> stringsErr
      Asm.StrToInt -> stringsErr
      Asm.Trace -> unsupported "trace"
      Asm.Dump -> unsupported "dump"
      Asm.Prealloc {} -> impossible
      Asm.CallClosures {} -> impossible
      Asm.TailCallClosures {} -> impossible

extendClosure :: (Members '[Compiler] r) => Natural -> Sem r ()
extendClosure extraArgsNum = do
  let pathToOldClosure = topOfStack ValueStack
      oldArgs = OpAddress # pathToOldClosure ++ closurePath ClosureArgs
      curArgsNum = OpAddress # pathToOldClosure ++ closurePath ClosureArgsNum
      extraArgs = stackSliceAsList ValueStack 1 extraArgsNum
  push (OpQuote # toNock ([] :: Path))
  push (OpAddress # indexInStack ValueStack 1 ++ closurePath ClosureArgsNum)
  appendRights
  moveTopFromTo ValueStack TempStack
  -- valueStack: [oldclosure ..]
  -- tempstack: [posOfArgsNil ..]
  pushOnto TempStack curArgsNum
  pushNatOnto TempStack extraArgsNum
  addOn TempStack
  pushOnto TempStack extraArgs
  -- valueStack: [oldclosure ..]
  -- tempstack: [xtraArgsList newArgsNum posOfArgsNil ..]
  let xtraArgs = OpAddress # topOfStack TempStack
      newArgsNum = OpAddress # indexInStack TempStack 1
      posOfArgsNil = OpAddress # indexInStack TempStack 2
      newClosure = makeClosure $ \case
        ClosureCode -> OpAddress # pathToOldClosure ++ closurePath ClosureCode
        ClosureTotalArgsNum -> OpAddress # pathToOldClosure ++ closurePath ClosureTotalArgsNum
        ClosureArgsNum -> newArgsNum
        ClosureArgs -> replaceSubterm' oldArgs posOfArgsNil xtraArgs
  pushOnto TempStack newClosure
  popN (1 + extraArgsNum)
  moveTopFromTo TempStack ValueStack
  popFromN 3 TempStack

constUnit :: Term Natural
constUnit = constVoid

constVoid :: Term Natural
constVoid = makeConstructor $ \case
  ConstructorTag -> OpQuote # toNock (0 :: Natural)
  ConstructorArgs -> remakeList []

pushConstructorFieldOnto :: (Members '[Compiler] r) => StackId -> Asm.DirectRef -> Natural -> Sem r ()
pushConstructorFieldOnto s refToConstr argIx =
  let path = directRefPath refToConstr ++ constructorPath ConstructorArgs ++ indexStack argIx
   in pushOnto s (OpAddress # path)

pushConstructorField :: (Members '[Compiler] r) => Asm.DirectRef -> Natural -> Sem r ()
pushConstructorField = pushConstructorFieldOnto ValueStack

directRefPath :: Asm.DirectRef -> Path
directRefPath = \case
  Asm.StackRef -> topOfStack ValueStack
  Asm.ArgRef a -> pathToArg (fromOffsetRef a)
  Asm.TempRef off -> indexInStack TempStack (fromOffsetRef off)

pushDirectRef :: (Members '[Compiler] r) => Asm.DirectRef -> Sem r ()
pushDirectRef = push . (OpAddress #) . directRefPath

allocClosure :: (Members '[Compiler] r) => FunctionId -> Natural -> Sem r ()
allocClosure funSym numArgs = do
  funPath <- getFunctionPath funSym
  funAri <- getFunctionArity funSym
  pushOnto TempStack (stackTake ValueStack numArgs)
  let closure = makeClosure $ \case
        ClosureCode -> OpAddress # funPath
        ClosureTotalArgsNum -> OpQuote # toNock funAri
        ClosureArgsNum -> OpQuote # toNock numArgs
        ClosureArgs -> OpAddress # topOfStack TempStack
  popNAndPushOnto ValueStack numArgs closure
  popFrom TempStack

closureArgsNum :: (Members '[Compiler] r) => Sem r ()
closureArgsNum = do
  let helper p = OpAddress # topOfStack ValueStack ++ closurePath p
  sub (helper ClosureTotalArgsNum) (helper ClosureArgsNum) pop

allocConstr :: (Members '[Compiler] r) => Asm.Tag -> Sem r ()
allocConstr tag = do
  numArgs <- getConstructorArity tag
  let args = [OpAddress # indexInStack ValueStack (pred i) | i <- [1 .. numArgs]]
      constr = goConstructor tag args
  pushOnto TempStack constr
  popN numArgs
  moveTopFromTo TempStack ValueStack

copyTopFromTo :: (Members '[Compiler] r) => StackId -> StackId -> Sem r ()
copyTopFromTo from toStack = pushOnto toStack (OpAddress # topOfStack from)

moveTopFromTo :: (Members '[Compiler] r) => StackId -> StackId -> Sem r ()
moveTopFromTo from toStack = do
  pushOnto toStack (OpAddress # topOfStack from)
  popFrom from

unsupported :: Text -> a
unsupported thing = error ("The Nockma backend does not support" <> thing)

stringsErr :: a
stringsErr = unsupported "strings"

-- | Computes a - b
sub :: (Members '[Compiler] r) => Term Natural -> Term Natural -> Sem r () -> Sem r ()
sub a b aux = do
  pushOnto TempStack b
  pushOnto TempStack a
  aux
  callStdlibOn TempStack StdlibSub
  moveTopFromTo TempStack ValueStack

seqTerms :: [Term Natural] -> Term Natural
seqTerms = foldl' step (OpAddress # emptyPath) . reverse
  where
    step :: Term Natural -> Term Natural -> Term Natural
    step acc t = OpSequence # t # acc

makeEmptyList :: Term Natural
makeEmptyList = makeList []

makeList :: [Term Natural] -> Term Natural
makeList ts = foldTerms (ts `prependList` pure (TermAtom nockNil))

remakeList :: (Foldable l) => l (Term Natural) -> Term Natural
remakeList ts = foldTerms (toList ts `prependList` pure (OpQuote # nockNil'))

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

execCompiler :: (Member (Reader CompilerCtx) r) => Sem (Compiler ': r) a -> Sem r (Term Natural)
execCompiler = fmap fst . runCompiler

runCompiler :: (Member (Reader CompilerCtx) r) => Sem (Compiler ': r) a -> Sem r (Term Natural, a)
runCompiler sem = do
  (ts, a) <- runOutputList (re sem)
  return (seqTerms ts, a)

runCompilerWith :: ConstructorArities -> [CompilerFunction] -> CompilerFunction -> (NonEmpty (Term Natural), Term Natural)
runCompilerWith constrs libFuns mainFun =
  let entryCommand :: (Members '[Compiler] r) => Sem r ()
      entryCommand = callFun (mainFun ^. compilerFunctionName) 0
      entryTerm =
        seqTerms
          . run
          . runReader compilerCtx
          . execOutputList
          . re
          $ entryCommand
      compiledFuns :: NonEmpty (Term Natural)
      compiledFuns =
        (# nockNil')
          <$> ( run
                  . runReader compilerCtx
                  . mapM (execCompiler . (^. compilerFunction))
                  $ allFuns
              )
   in (compiledFuns, entryTerm)
  where
    allFuns :: NonEmpty CompilerFunction
    allFuns = mainFun :| libFuns ++ (builtinFunction <$> allElements)

    compilerCtx :: CompilerCtx
    compilerCtx =
      CompilerCtx
        { _compilerFunctionInfos = functionInfos,
          _compilerConstructorArities = constrs
        }

    functionInfos :: HashMap FunctionId FunctionInfo
    functionInfos = hashMap (run (runInputNaturals (toList <$> userFunctions)))

    userFunctions :: (Members '[Input Natural] r) => Sem r (NonEmpty (FunctionId, FunctionInfo))
    userFunctions = forM allFuns $ \CompilerFunction {..} -> do
      i <- input
      return
        ( _compilerFunctionName,
          FunctionInfo
            { _functionPath = indexInStack FunctionsLibrary i,
              _functionArity = _compilerFunctionArity
            }
        )

builtinFunction :: BuiltinFunctionId -> CompilerFunction
builtinFunction = \case
  BuiltinAppendRights ->
    CompilerFunction
      { _compilerFunctionName = BuiltinFunction BuiltinAppendRights,
        _compilerFunctionArity = 2, -- args: n pos
        _compilerFunction = do
          push (OpAddress # pathToArg 0)
          pow2
          push (OpAddress # pathToArg 1)
          pushNat 1
          add
          mul
          dec
          asmReturn
      }
  BuiltinPow2 ->
    CompilerFunction
      { _compilerFunctionName = BuiltinFunction BuiltinPow2,
        _compilerFunctionArity = 1,
        _compilerFunction = do
          pushNat 1 -- acc
          push (OpAddress # pathToArg 0)
          callFun (BuiltinFunction BuiltinPow2Go) 2
          asmReturn
      }
  BuiltinPow2Go ->
    CompilerFunction
      { _compilerFunctionName = BuiltinFunction BuiltinPow2Go,
        _compilerFunctionArity = 2, -- args: n acc
        _compilerFunction = do
          push (OpAddress # pathToArg 1)
          push (OpAddress # pathToArg 0)
          copyTopFromTo ValueStack TempStack
          pushNat 0
          testEq
          let baseCase :: (Members '[Compiler] r) => Sem r ()
              baseCase = popFrom TempStack >> asmReturn
              recCase :: (Members '[Compiler] r) => Sem r ()
              recCase = do
                pushNat 2
                mul
                moveTopFromTo TempStack ValueStack
                dec
                callHelper True (Just (BuiltinFunction BuiltinPow2Go)) 2
          branch baseCase recCase
      }

callEnum :: (Enum funId, Members '[Compiler] r) => funId -> Natural -> Sem r ()
callEnum = callFun . UserFunction . Asm.defaultSymbol . fromIntegral . fromEnum

callFun :: (Members '[Compiler] r) => FunctionId -> Natural -> Sem r ()
callFun = callHelper False . Just

tcallFun :: (Members '[Compiler] r) => FunctionId -> Natural -> Sem r ()
tcallFun = callHelper True . Just

getFunctionPath' :: (Members '[Reader CompilerCtx] r) => FunctionId -> Sem r Path
getFunctionPath' funName = asks (^?! compilerFunctionInfos . at funName . _Just . functionPath)

-- | obj[relPath] := newVal
-- relPath is relative to obj
replaceSubterm :: Term Natural -> Path -> Term Natural -> Term Natural
replaceSubterm obj relPath newVal = OpReplace # (relPath # newVal) # obj

evaluated :: Term Natural -> Term Natural
evaluated t = OpApply # (OpAddress # emptyPath) # t

-- | The same as replaceSubterm but the path is a cell that is evaluated.
-- i.e. replaceSubterm a p b = replaceSubterm' a (quote p) b
replaceSubterm' :: Term Natural -> Term Natural -> Term Natural -> Term Natural
replaceSubterm' obj relPath newVal =
  evaluated $ (OpQuote # OpReplace) # ((relPath # (OpQuote # newVal)) # (OpQuote # obj))

sre :: (Members '[Output (Term Natural), Reader CompilerCtx] r) => Sem (Compiler ': r) x -> Sem r x
sre = subsume . re

-- | funName is Nothing when we call a closure at the top of the stack
callHelper' :: (Members '[Output (Term Natural), Reader CompilerCtx] r) => Bool -> Maybe FunctionId -> Natural -> Sem r ()
callHelper' isTail funName funArgsNum = do
  let isClosure = isNothing funName
  -- 1. Obtain the path to the function
  funPath <- maybe (return (topOfStack ValueStack)) getFunctionPath' funName

  -- 2.
  --   i. Take a copy of the value stack without the arguments to the function
  --   ii. Push this copy to the call stack
  let storeOnStack
        | isTail = replaceOnStack
        | otherwise = pushOnStack
      numToPop
        | isClosure = 1 + funArgsNum
        | otherwise = funArgsNum
  output (storeOnStack CallStack (stackPop ValueStack numToPop))

  -- 3.
  --  i. Take a copy of the function from the function library
  --  ii. Replace its argument area with the arguments from the value stack
  --  iii. Push this copy to the current function stack

  -- Setup function to call with its arguments
  if
      | isClosure -> sre $ do
          push (OpQuote # toNock ([] :: Path))
          push (OpAddress # indexInStack ValueStack 1 ++ closurePath ClosureArgsNum)
          appendRights
          moveTopFromTo ValueStack TempStack
          let closurepath = topOfStack ValueStack
              posOfArgsNil = OpAddress # topOfStack TempStack
              oldArgs = OpAddress # closurepath ++ closurePath ClosureArgs
              xtraArgs = stackSliceAsList ValueStack 1 funArgsNum
              allArgs = replaceSubterm' oldArgs posOfArgsNil xtraArgs
              funWithArgs = (OpAddress # closurepath ++ closurePath ClosureCode) # allArgs
          output (storeOnStack CurrentFunction funWithArgs)
          popFrom TempStack
      | otherwise -> do
          let funWithArgs = replaceSubterm (OpAddress # funPath) [R] (stackTake ValueStack funArgsNum)
          output (storeOnStack CurrentFunction funWithArgs)

  -- 4. Replace the value stack with nil
  output (resetStack ValueStack)

  -- 5. Evaluate the function in the context of the whole nock stack
  -- 6. See documentation for asmReturn'
  output (OpCall # ((topOfStack CurrentFunction ++ [L]) # (OpAddress # emptyPath)))

asmReturn' :: (Members '[Output (Term Natural), Reader CompilerCtx] r) => Sem r ()
asmReturn' = do
  -- Restore the previous value stack (created in call'.2.). i.e copy the previous value stack
  -- from the call stack and push the result (the head of the current value stack) to it.
  output (replaceStack ValueStack ((OpAddress # topOfStack ValueStack) # (OpAddress # topOfStack CallStack)))

  -- discard the 'activation' frame
  sre $ do
    popFrom CallStack
    popFrom CurrentFunction

testEq :: (Members '[Compiler] r) => Sem r ()
testEq = testEqOn ValueStack

testEqOn' :: (Members '[Output (Term Natural)] r) => StackId -> Sem r ()
testEqOn' s = output (replaceOnStackN 2 s (OpEq # stackSliceAsCell s 0 1))

incrementOn' :: (Members '[Output (Term Natural)] r) => StackId -> Sem r ()
incrementOn' s = output (replaceOnStack s (OpInc # stackSliceAsCell s 0 0))

callStdlib :: (Members '[Compiler] r) => StdlibFunction -> Sem r ()
callStdlib = callStdlibOn ValueStack

callStdlibOn' :: (Members '[Output (Term Natural)] r) => StackId -> StdlibFunction -> Sem r ()
callStdlibOn' s f = do
  let fNumArgs = stdlibNumArgs f
      fPath = stdlibPath f
      decodeFn = OpCall # (fPath # (OpAddress # stackPath StandardLibrary))
      arguments = OpSequence # (OpAddress # [R]) # stdlibStackTake s fNumArgs
      extractResult = (OpAddress # [L]) # (OpAddress # [R, R])
      callFn = OpPush # (OpCall # [L] # (OpReplace # ([R, L] # arguments) # (OpAddress # [L]))) # extractResult

  output (OpPush # decodeFn # callFn)
  output (replaceTopStackN fNumArgs s)
  where
    stdlibStackTake :: StackId -> Natural -> Term Natural
    stdlibStackTake sn n =
      foldTerms
        ( nonEmpty'
            (take (fromIntegral n) [OpAddress # indexInStack sn i | i <- [0 ..]])
        )

save' ::
  (Functor f, Members '[Output (Term Natural), Reader CompilerCtx] r) =>
  Bool ->
  m () ->
  Sem (WithTactics Compiler f m r) (f ())
save' isTail m = do
  pushOntoH TempStack (OpAddress # topOfStack ValueStack)
  popFromH ValueStack
  runT m >>= raise . execCompiler >>= output >>= pureT
  if
      | isTail -> pureT ()
      | otherwise -> popFromH TempStack

constructorTagToTerm :: Asm.Tag -> Term Natural
constructorTagToTerm = \case
  Asm.UserTag _ i -> OpQuote # toNock (fromIntegral i :: Natural)
  Asm.BuiltinTag b -> case b of
    Asm.TagTrue -> error "TODO"
    Asm.TagFalse -> error "TODO"
    Asm.TagReturn -> impossible
    Asm.TagBind -> impossible
    Asm.TagWrite -> impossible
    Asm.TagReadLn -> impossible

caseCmd ::
  (Members '[Compiler] r) =>
  Maybe (Sem r ()) ->
  [(Asm.Tag, Sem r ())] ->
  Sem r ()
caseCmd defaultBranch = \case
  [] -> sequence_ defaultBranch
  (tag, b) : bs -> do
    -- push the constructor tag at the top
    push (OpAddress # topOfStack ValueStack ++ constructorPath ConstructorTag)
    push (constructorTagToTerm tag)
    testEq
    branch b (caseCmd defaultBranch bs)

branch' ::
  (Functor f, Members '[Output (Term Natural), Reader CompilerCtx] r) =>
  m () ->
  m () ->
  Sem (WithTactics Compiler f m r) (f ())
branch' t f = do
  termT <- runT t >>= raise . execCompiler . (pop >>)
  termF <- runT f >>= raise . execCompiler . (pop >>)
  (output >=> pureT) (OpIf # (OpAddress # pathInStack ValueStack [L]) # termT # termF)

getFunctionArity' :: (Members '[Reader CompilerCtx] r) => FunctionId -> Sem r Natural
getFunctionArity' s = asks (^?! compilerFunctionInfos . at s . _Just . functionArity)

getConstructorArity' :: (Members '[Reader CompilerCtx] r) => Asm.Tag -> Sem r Natural
getConstructorArity' tag = asks (^?! compilerConstructorArities . at tag . _Just)

re :: (Member (Reader CompilerCtx) r) => Sem (Compiler ': r) a -> Sem (Output (Term Natural) ': r) a
re = reinterpretH $ \case
  PushOnto s n -> pushOntoH s n
  PopFromN n s -> popFromNH n s
  PopNAndPushOnto s n t -> popNAndPushOnto' s n t >>= pureT
  Verbatim s -> outputT s
  CallHelper isTail funName funArgsNum -> callHelper' isTail funName funArgsNum >>= pureT
  IncrementOn s -> incrementOn' s >>= pureT
  Branch t f -> branch' t f
  Save isTail m -> save' isTail m
  CallStdlibOn s f -> callStdlibOn' s f >>= pureT
  AsmReturn -> asmReturn' >>= pureT
  TestEqOn s -> testEqOn' s >>= pureT
  GetConstructorArity s -> getConstructorArity' s >>= pureT
  GetFunctionArity s -> getFunctionArity' s >>= pureT
  GetFunctionPath s -> getFunctionPath' s >>= pureT
  Crash -> outputT (OpAddress # OpAddress # OpAddress)

outputT :: (Functor f, Member (Output (Term Natural)) r) => Term Natural -> Sem (WithTactics e f m r) (f ())
outputT = output >=> pureT

pushOntoH ::
  (Functor f, Member (Output (Term Natural)) r) =>
  StackId ->
  Term Natural ->
  Sem (WithTactics e f m r) (f ())
pushOntoH s n = outputT (pushOnStack s n)

popFromH ::
  (Functor f, Member (Output (Term Natural)) r) =>
  StackId ->
  Sem (WithTactics e f m r) (f ())
popFromH s = outputT (popStack s)

popFromNH ::
  (Functor f, Member (Output (Term Natural)) r) =>
  Natural ->
  StackId ->
  Sem (WithTactics e f m r) (f ())
popFromNH n s = outputT (popStackN n s)

mul :: (Members '[Compiler] r) => Sem r ()
mul = mulOn ValueStack

mulOn :: (Members '[Compiler] r) => StackId -> Sem r ()
mulOn s = callStdlibOn s StdlibMul

addOn :: (Members '[Compiler] r) => StackId -> Sem r ()
addOn s = callStdlibOn s StdlibAdd

-- | arg order: push path >> push n
appendRights :: (Members '[Compiler] r) => Sem r ()
appendRights = callFun (BuiltinFunction BuiltinAppendRights) 2

pow2 :: (Members '[Compiler] r) => Sem r ()
pow2 = callFun (BuiltinFunction BuiltinPow2) 1

add :: (Members '[Compiler] r) => Sem r ()
add = addOn ValueStack

dec :: (Members '[Compiler] r) => Sem r ()
dec = callStdlib StdlibDec

increment :: (Members '[Compiler] r) => Sem r ()
increment = incrementOn ValueStack

popFrom :: (Members '[Compiler] r) => StackId -> Sem r ()
popFrom = popFromN 1

popN :: (Members '[Compiler] r) => Natural -> Sem r ()
popN n = popFromN n ValueStack

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
stackSliceAsCell sn a b = foldTerms (stackSliceHelper sn a b)

-- | Takes a slice of a stack. Both indices are inclusive
stackSliceAsList :: StackId -> Natural -> Natural -> Term Natural
stackSliceAsList sn fromIx toIx = remakeList (stackSliceHelper sn fromIx toIx)

pushOnStack :: StackId -> Term Natural -> Term Natural
pushOnStack s t = OpPush # t # topStack s

popNAndPushOnto' :: (Member (Output (Term Natural)) r) => StackId -> Natural -> Term Natural -> Sem r ()
popNAndPushOnto' s num t = output (replaceOnStackN num s t)

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

compileAndRunNock :: ConstructorArities -> [CompilerFunction] -> CompilerFunction -> Term Natural
compileAndRunNock constrs funs mainfun =
  let (functionTerms, t) = runCompilerWith constrs funs mainfun
   in evalCompiledNock (toList functionTerms) t

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
