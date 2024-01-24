module Juvix.Compiler.Nockma.Translation.FromAsm where

import Juvix.Compiler.Asm.Data.InfoTable qualified as Asm
import Juvix.Compiler.Asm.Language qualified as Asm
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Stdlib
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Tree.Language.Rep
import Juvix.Prelude hiding (Atom, Path)

nockmaMemRep :: MemRep -> NockmaMemRep
nockmaMemRep = \case
  MemRepTuple -> NockmaMemRepTuple
  MemRepConstr -> NockmaMemRepConstr
  MemRepTag -> NockmaMemRepConstr
  MemRepUnit -> NockmaMemRepConstr
  MemRepUnpacked {} -> NockmaMemRepConstr

data NockmaMemRep
  = NockmaMemRepConstr
  | NockmaMemRepTuple

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

newtype CompilerOptions = CompilerOptions
  {_compilerOptionsEnableTrace :: Bool}

fromEntryPoint :: EntryPoint -> CompilerOptions
fromEntryPoint EntryPoint {..} =
  CompilerOptions
    { _compilerOptionsEnableTrace = _entryPointDebug
    }

data FunctionInfo = FunctionInfo
  { _functionInfoPath :: Path,
    _functionInfoArity :: Natural
  }

data CompilerCtx = CompilerCtx
  { _compilerFunctionInfos :: HashMap FunctionId FunctionInfo,
    _compilerConstructorInfos :: ConstructorInfos,
    _compilerOptions :: CompilerOptions
  }

data ConstructorInfo = ConstructorInfo
  { _constructorInfoArity :: Natural,
    _constructorInfoMemRep :: NockmaMemRep
  }

type ConstructorInfos = HashMap Asm.Tag ConstructorInfo

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
  | AuxStack
  | FrameStack
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

data ActivationFramePathId
  = ActivationFrameValueStack
  | ActivationFrameTempStack
  | ActivationFrameAuxStack
  deriving stock (Bounded, Enum)

activationFramePath :: ActivationFramePathId -> Path
activationFramePath = pathFromEnum

data FunctionPathId
  = FunctionCode
  | FunctionArgs

functionPath :: FunctionPathId -> Path
functionPath = \case
  FunctionCode -> [L]
  FunctionArgs -> [R]

-- | The stdlib paths are obtained using scripts/nockma-stdlib-parser.sh
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
  TraceTerm :: Term Natural -> Compiler m ()
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
  GetConstructorInfo :: Asm.Tag -> Compiler m ConstructorInfo
  GetFunctionArity :: FunctionId -> Compiler m Natural
  GetFunctionPath :: FunctionId -> Compiler m Path

stackPath :: StackId -> Path
stackPath s = indexStack (fromIntegral (fromEnum s))

indexTuple :: Natural -> Natural -> Path
indexTuple len idx =
  let lastL
        | idx == len - 1 = []
        | otherwise = [L]
   in replicate idx R ++ lastL

indexStack :: Natural -> Path
indexStack idx = replicate idx R ++ [L]

indexInPath :: Path -> Natural -> Path
indexInPath p idx = p ++ indexStack idx

topOfStack :: StackId -> Path
topOfStack s = indexInStack s 0

indexInStack :: StackId -> Natural -> Path
indexInStack s idx = stackPath s ++ indexStack idx

pathToArgumentsArea :: Path
pathToArgumentsArea = topOfStack CurrentFunction ++ functionPath FunctionArgs

pathToArg :: Natural -> Path
pathToArg = indexInPath pathToArgumentsArea

-- | Construct a path rooted at he head of a named stack
pathInStack :: StackId -> Path -> Path
pathInStack s p = stackPath s ++ p

makeSem ''Compiler
makeLenses ''CompilerOptions
makeLenses ''CompilerFunction
makeLenses ''CompilerCtx
makeLenses ''ConstructorInfo
makeLenses ''FunctionInfo

termFromParts :: (Bounded p, Enum p) => (p -> Term Natural) -> Term Natural
termFromParts f = remakeList [f pi | pi <- allElements]

makeClosure :: (ClosurePathId -> Term Natural) -> Term Natural
makeClosure = termFromParts

makeConstructor :: (ConstructorPathId -> Term Natural) -> Term Natural
makeConstructor = termFromParts

makeActivationFrame :: (ActivationFramePathId -> Term Natural) -> Term Natural
makeActivationFrame = termFromParts

makeFunction :: (FunctionPathId -> Term Natural) -> Term Natural
makeFunction f = f FunctionCode # f FunctionArgs

foldTerms :: NonEmpty (Term Natural) -> Term Natural
foldTerms = foldr1 (#)

-- | Use `Asm.toNockma` before calling this function
fromAsmTable :: (Members '[Error JuvixError, Reader CompilerOptions] r) => Asm.InfoTable -> Sem r (Cell Natural)
fromAsmTable t = case t ^. Asm.infoMainFunction of
  Just mainFun -> do
    opts <- ask
    return (fromAsm opts mainFun t)
  Nothing -> throw @JuvixError (error "TODO")
  where
    fromAsm :: CompilerOptions -> Asm.Symbol -> Asm.InfoTable -> Cell Natural
    fromAsm opts mainSym Asm.InfoTable {..} =
      let funs = map compileFunction allFunctions
          mkConstructorInfo :: Asm.ConstructorInfo -> ConstructorInfo
          mkConstructorInfo ci@Asm.ConstructorInfo {..} =
            ConstructorInfo
              { _constructorInfoArity = fromIntegral _constructorArgsNum,
                _constructorInfoMemRep = nockmaMemRep (memRep ci (getInductiveInfo (ci ^. Asm.constructorInductive)))
              }
          constrs :: ConstructorInfos
          constrs = mkConstructorInfo <$> _infoConstrs

          getInductiveInfo :: Symbol -> Asm.InductiveInfo
          getInductiveInfo s = _infoInductives ^?! at s . _Just
       in runCompilerWith opts constrs funs mainFun
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

        memRep :: Asm.ConstructorInfo -> Asm.InductiveInfo -> Asm.MemRep
        memRep ci ind
          | numArgs >= 1 && numConstrs == 1 = MemRepTuple
          | otherwise = MemRepConstr
          where
            numConstrs = length (ind ^. Asm.inductiveConstructors)
            numArgs = ci ^. Asm.constructorArgsNum

fromOffsetRef :: Asm.OffsetRef -> Natural
fromOffsetRef = fromIntegral . (^. Asm.offsetRefOffset)

-- | Generic constructors are encoded as [tag args], where args is a
-- nil terminated list.
goConstructor :: NockmaMemRep -> Asm.Tag -> [Term Natural] -> Term Natural
goConstructor mr t args = case t of
  Asm.BuiltinTag b -> makeConstructor $ \case
    ConstructorTag -> builtinTagToTerm b
    ConstructorArgs -> remakeList []
  Asm.UserTag tag -> case mr of
    NockmaMemRepConstr ->
      makeConstructor $ \case
        ConstructorTag -> OpQuote # (fromIntegral (tag ^. Asm.tagUserWord) :: Natural)
        ConstructorArgs -> remakeList args
    NockmaMemRepTuple -> foldTerms (nonEmpty' args)

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
      Asm.Constant (Asm.ConstInt i)
        | i < 0 -> unsupported "negative numbers"
        | otherwise -> pushNat (fromInteger i)
      Asm.Constant (Asm.ConstBool i) -> push (nockBoolLiteral i)
      Asm.Constant Asm.ConstString {} -> stringsErr
      Asm.Constant Asm.ConstUnit -> push constUnit
      Asm.Constant Asm.ConstVoid -> push constVoid
      Asm.Ref r -> pushMemValue r
      where
        pushMemValue :: Asm.MemRef -> Sem r ()
        pushMemValue = \case
          Asm.DRef r -> pushDirectRef r
          Asm.ConstrRef r ->
            pushConstructorField
              (r ^. Asm.fieldTag)
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

    goDump :: Sem r ()
    goDump = do
      dumpStack ValueStack
      dumpStack AuxStack
      dumpStack TempStack
      dumpStack FrameStack

    goTrace :: Sem r ()
    goTrace = traceTerm (OpAddress # topOfStack ValueStack)

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
      Asm.Trace -> goTrace
      Asm.Dump -> goDump
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
  moveTopFromTo ValueStack AuxStack
  -- valueStack: [oldclosure ..]
  -- tempstack: [posOfArgsNil ..]
  pushOnto AuxStack curArgsNum
  pushNatOnto AuxStack extraArgsNum
  addOn AuxStack
  pushOnto AuxStack extraArgs
  -- valueStack: [oldclosure ..]
  -- tempstack: [xtraArgsList newArgsNum posOfArgsNil ..]
  let xtraArgs = OpAddress # topOfStack AuxStack
      newArgsNum = OpAddress # indexInStack AuxStack 1
      posOfArgsNil = OpAddress # indexInStack AuxStack 2
      newClosure = makeClosure $ \case
        ClosureCode -> OpAddress # pathToOldClosure ++ closurePath ClosureCode
        ClosureTotalArgsNum -> OpAddress # pathToOldClosure ++ closurePath ClosureTotalArgsNum
        ClosureArgsNum -> newArgsNum
        ClosureArgs -> replaceSubterm' oldArgs posOfArgsNil xtraArgs
  pushOnto AuxStack newClosure
  popN (1 + extraArgsNum)
  moveTopFromTo AuxStack ValueStack
  popFromN 3 AuxStack

constUnit :: Term Natural
constUnit = constVoid

constVoid :: Term Natural
constVoid = makeConstructor $ \case
  ConstructorTag -> OpQuote # toNock (0 :: Natural)
  ConstructorArgs -> remakeList []

pushConstructorFieldOnto ::
  (Members '[Compiler] r) =>
  StackId ->
  Asm.Tag ->
  Asm.DirectRef ->
  Natural ->
  Sem r ()
pushConstructorFieldOnto s tag refToConstr argIx = do
  info <- getConstructorInfo tag
  let memrep = info ^. constructorInfoMemRep
      arity = info ^. constructorInfoArity
      path = case memrep of
        NockmaMemRepConstr ->
          directRefPath refToConstr
            ++ constructorPath ConstructorArgs
            ++ indexStack argIx
        NockmaMemRepTuple ->
          directRefPath refToConstr
            ++ indexTuple arity argIx
  pushOnto s (OpAddress # path)

pushConstructorField :: (Members '[Compiler] r) => Asm.Tag -> Asm.DirectRef -> Natural -> Sem r ()
pushConstructorField = pushConstructorFieldOnto ValueStack

directRefPath :: Asm.DirectRef -> Path
directRefPath = \case
  Asm.ArgRef a -> pathToArg (fromOffsetRef a)
  Asm.TempRef Asm.RefTemp {..} -> tempRefPath (fromIntegral (fromJust _refTempTempHeight)) (fromOffsetRef _refTempOffsetRef)

pushDirectRef :: (Members '[Compiler] r) => Asm.DirectRef -> Sem r ()
pushDirectRef = push . (OpAddress #) . directRefPath

tempRefPath :: Natural -> Natural -> Path
tempRefPath tempHeight off = indexInStack TempStack (tempHeight - off - 1)

pushTempRef :: (Members '[Compiler] r) => Natural -> Natural -> Sem r ()
pushTempRef tempHeight = push . (OpAddress #) . tempRefPath tempHeight

allocClosure :: (Members '[Compiler] r) => FunctionId -> Natural -> Sem r ()
allocClosure funSym numArgs = do
  funPath <- getFunctionPath funSym
  funAri <- getFunctionArity funSym
  pushOnto AuxStack (stackTake ValueStack numArgs)
  let closure = makeClosure $ \case
        ClosureCode -> OpAddress # funPath
        ClosureTotalArgsNum -> OpQuote # toNock funAri
        ClosureArgsNum -> OpQuote # toNock numArgs
        ClosureArgs -> OpAddress # topOfStack AuxStack
  popNAndPushOnto ValueStack numArgs closure
  popFrom AuxStack

closureArgsNum :: (Members '[Compiler] r) => Sem r ()
closureArgsNum = do
  let helper p = OpAddress # topOfStack ValueStack ++ closurePath p
  sub (helper ClosureTotalArgsNum) (helper ClosureArgsNum) pop

allocConstr :: (Members '[Compiler] r) => Asm.Tag -> Sem r ()
allocConstr tag = do
  info <- getConstructorInfo tag
  let numArgs = info ^. constructorInfoArity
      memrep = info ^. constructorInfoMemRep
      args = [OpAddress # indexInStack ValueStack (pred i) | i <- [1 .. numArgs]]
      constr = goConstructor memrep tag args
  pushOnto AuxStack constr
  popN numArgs
  moveTopFromTo AuxStack ValueStack

copyTopFromTo :: (Members '[Compiler] r) => StackId -> StackId -> Sem r ()
copyTopFromTo from toStack = pushOnto toStack (OpAddress # topOfStack from)

moveTopFromTo :: (Members '[Compiler] r) => StackId -> StackId -> Sem r ()
moveTopFromTo from toStack = do
  pushOnto toStack (OpAddress # topOfStack from)
  popFrom from

unsupported :: Text -> a
unsupported thing = error ("The Nockma backend does not support " <> thing)

stringsErr :: a
stringsErr = unsupported "strings"

-- | Computes a - b
sub :: (Members '[Compiler] r) => Term Natural -> Term Natural -> Sem r () -> Sem r ()
sub a b aux = do
  pushOnto AuxStack b
  pushOnto AuxStack a
  aux
  callStdlibOn AuxStack StdlibSub
  moveTopFromTo AuxStack ValueStack

seqTerms :: [Term Natural] -> Term Natural
seqTerms = foldl' (flip (>>#)) (OpAddress # emptyPath) . reverse

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
      FrameStack -> nockNil'
      TempStack -> nockNil'
      AuxStack -> nockNil'
      StandardLibrary -> stdlib
      FunctionsLibrary -> makeList defs

push :: (Members '[Compiler] r) => Term Natural -> Sem r ()
push = pushOnto ValueStack

execCompilerList :: (Member (Reader CompilerCtx) r) => Sem (Compiler ': r) a -> Sem r [Term Natural]
execCompilerList = fmap fst . runCompilerList

runCompilerList :: (Member (Reader CompilerCtx) r) => Sem (Compiler ': r) a -> Sem r ([Term Natural], a)
runCompilerList sem = do
  (ts, a) <- runOutputList (re sem)
  return (ts, a)

execCompiler :: (Member (Reader CompilerCtx) r) => Sem (Compiler ': r) a -> Sem r (Term Natural)
execCompiler = fmap fst . runCompiler

runCompiler :: (Member (Reader CompilerCtx) r) => Sem (Compiler ': r) a -> Sem r (Term Natural, a)
runCompiler sem = do
  (ts, a) <- runOutputList (re sem)
  return (seqTerms ts, a)

runCompilerWith :: CompilerOptions -> ConstructorInfos -> [CompilerFunction] -> CompilerFunction -> Cell Natural
runCompilerWith opts constrs libFuns mainFun =
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
        makeFunction'
          <$> ( run
                  . runReader compilerCtx
                  . mapM (execCompiler . (^. compilerFunction))
                  $ allFuns
              )
      makeFunction' :: Term Natural -> Term Natural
      makeFunction' c = makeFunction $ \case
        FunctionCode -> c
        FunctionArgs -> nockNil'
   in Cell (initStack (toList compiledFuns)) entryTerm
  where
    allFuns :: NonEmpty CompilerFunction
    allFuns = mainFun :| libFuns ++ (builtinFunction <$> allElements)

    compilerCtx :: CompilerCtx
    compilerCtx =
      CompilerCtx
        { _compilerFunctionInfos = functionInfos,
          _compilerConstructorInfos = constrs,
          _compilerOptions = opts
        }

    functionInfos :: HashMap FunctionId FunctionInfo
    functionInfos = hashMap (run (runInputNaturals (toList <$> userFunctions)))

    userFunctions :: (Members '[Input Natural] r) => Sem r (NonEmpty (FunctionId, FunctionInfo))
    userFunctions = forM allFuns $ \CompilerFunction {..} -> do
      i <- input
      return
        ( _compilerFunctionName,
          FunctionInfo
            { _functionInfoPath = indexInStack FunctionsLibrary i,
              _functionInfoArity = _compilerFunctionArity
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
          copyTopFromTo ValueStack AuxStack
          pushNat 0
          testEq
          let baseCase :: (Members '[Compiler] r) => Sem r ()
              baseCase = popFrom AuxStack >> asmReturn
              recCase :: (Members '[Compiler] r) => Sem r ()
              recCase = do
                pushNat 2
                mul
                moveTopFromTo AuxStack ValueStack
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
getFunctionPath' funName = asks (^?! compilerFunctionInfos . at funName . _Just . functionInfoPath)

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
callHelper' ::
  (Members '[Output (Term Natural), Reader CompilerCtx] r) =>
  Bool ->
  Maybe FunctionId ->
  Natural ->
  Sem r ()
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

  unless isTail $ do
    let frame = makeActivationFrame $ \case
          ActivationFrameValueStack -> stackPop ValueStack numToPop
          ActivationFrameTempStack -> OpAddress # stackPath TempStack
          ActivationFrameAuxStack -> OpAddress # stackPath AuxStack
    output (pushOnStack FrameStack frame)

  -- 3.
  --  i. Take a copy of the function from the function library
  --  ii. Replace its argument area with the arguments from the value stack
  --  iii. Push this copy to the current function stack

  -- Setup function to call with its arguments
  -- given n, we compute [R..R] of length n
  if
      | isClosure -> sre $ do
          push (OpQuote # toNock ([] :: Path))
          push (OpAddress # indexInStack ValueStack 1 ++ closurePath ClosureArgsNum)
          appendRights
          moveTopFromTo ValueStack AuxStack
          let closurepath = topOfStack ValueStack
              posOfArgsNil = OpAddress # topOfStack AuxStack
              oldArgs = OpAddress # closurepath ++ closurePath ClosureArgs
              xtraArgs = stackSliceAsList ValueStack 1 funArgsNum
              allArgs = replaceSubterm' oldArgs posOfArgsNil xtraArgs
              funCode = OpAddress # closurepath ++ closurePath ClosureCode
              funWithArgs = replaceSubterm funCode (functionPath FunctionArgs) allArgs
          output (storeOnStack CurrentFunction funWithArgs)
          popFrom AuxStack
      | otherwise -> do
          let funWithArgs = replaceSubterm (OpAddress # funPath) (functionPath FunctionArgs) (stackTake ValueStack funArgsNum)
          output (storeOnStack CurrentFunction funWithArgs)

  -- 4. Replace the value stack with nil
  output (resetStack ValueStack)
  output (resetStack TempStack)
  output (resetStack AuxStack)

  -- 5. Evaluate the function in the context of the whole nock stack
  -- 6. See documentation for asmReturn'
  output (OpCall # ((topOfStack CurrentFunction ++ functionPath FunctionCode) # (OpAddress # emptyPath)))

asmReturn' :: (Members '[Output (Term Natural), Reader CompilerCtx] r) => Sem r ()
asmReturn' = do
  -- Restore the previous value stack (created in call'.2.). i.e copy the previous value stack
  -- from the call stack and push the result (the head of the current value stack) to it.

  output
    ( replaceStack
        ValueStack
        ( (OpAddress # topOfStack ValueStack)
            # (OpAddress # topOfStack FrameStack ++ activationFramePath ActivationFrameValueStack)
        )
    )

  output
    ( replaceStack
        TempStack
        (OpAddress # topOfStack FrameStack ++ activationFramePath ActivationFrameTempStack)
    )

  output
    ( replaceStack
        AuxStack
        (OpAddress # topOfStack FrameStack ++ activationFramePath ActivationFrameAuxStack)
    )

  -- discard the 'activation' frame
  sre $ do
    popFrom FrameStack
    popFrom CurrentFunction

testEq :: (Members '[Compiler] r) => Sem r ()
testEq = testEqOn ValueStack

testEqOn' :: (Members '[Output (Term Natural)] r) => StackId -> Sem r ()
testEqOn' s = output (replaceOnStackN 2 s (OpEq # stackSliceAsCell s 0 1))

dumpStack :: (Members '[Compiler] r) => StackId -> Sem r ()
dumpStack t = traceTerm (OpAddress # stackPath t)

traceTerm' :: (Members '[Output (Term Natural)] r) => Term Natural -> Sem r ()
traceTerm' t =
  let iden = OpAddress # ([] :: Path)
   in output (OpTrace # t # iden)

incrementOn' :: (Members '[Output (Term Natural)] r) => StackId -> Sem r ()
incrementOn' s = output (replaceOnStack s (OpInc # stackSliceAsCell s 0 0))

callStdlib :: (Members '[Compiler] r) => StdlibFunction -> Sem r ()
callStdlib = callStdlibOn ValueStack

callStdlibOn' :: (Members '[Output (Term Natural)] r) => StackId -> StdlibFunction -> Sem r ()
callStdlibOn' s f = do
  let fNumArgs = stdlibNumArgs f
      fPath = stdlibPath f
      decodeFn = OpCall # (fPath # (OpAddress # stackPath StandardLibrary))
      preargs = stdlibStackTake s fNumArgs
      arguments = OpSequence # (OpAddress # [R]) # preargs
      extractResult = (OpAddress # [L]) # (OpAddress # [R, R])
      callFn = OpPush # (OpCall # [L] # (OpReplace # ([R, L] # arguments) # (OpAddress # [L]))) # extractResult
      meta =
        StdlibCall
          { _stdlibCallArgs = preargs,
            _stdlibCallFunction = f
          }

      callCell = set cellCall (Just meta) (OpPush #. (decodeFn # callFn))

  output (toNock callCell)
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
  runT m >>= raise . execCompilerList >>= mapM_ output >>= pureT
  if
      | isTail -> pureT ()
      | otherwise -> popFromH TempStack

builtinTagToTerm :: Asm.BuiltinDataTag -> Term Natural
builtinTagToTerm = \case
  Asm.TagTrue -> nockBoolLiteral True
  Asm.TagFalse -> nockBoolLiteral False
  Asm.TagReturn -> impossible
  Asm.TagBind -> impossible
  Asm.TagWrite -> impossible
  Asm.TagReadLn -> impossible

constructorTagToTerm :: Asm.Tag -> Term Natural
constructorTagToTerm = \case
  Asm.UserTag t -> OpQuote # toNock (fromIntegral (t ^. Asm.tagUserWord) :: Natural)
  Asm.BuiltinTag b -> builtinTagToTerm b

caseCmd ::
  forall r.
  (Members '[Compiler] r) =>
  Maybe (Sem r ()) ->
  [(Asm.Tag, Sem r ())] ->
  Sem r ()
caseCmd defaultBranch = \case
  [] -> sequence_ defaultBranch
  (tag, b) : bs -> do
    rep <- getConstructorMemRep tag
    case rep of
      NockmaMemRepConstr -> goRepConstr tag b bs
      NockmaMemRepTuple
        | null bs, isNothing defaultBranch -> b
        | otherwise -> error "redundant branch. Impossible?"
  where
    goRepConstr :: Asm.Tag -> Sem r () -> [(Asm.Tag, Sem r ())] -> Sem r ()
    goRepConstr tag b bs = do
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
  (output >=> pureT) (OpIf # (OpAddress # topOfStack ValueStack) # termT # termF)

getFunctionArity' :: (Members '[Reader CompilerCtx] r) => FunctionId -> Sem r Natural
getFunctionArity' s = asks (^?! compilerFunctionInfos . at s . _Just . functionInfoArity)

getConstructorInfo' :: (Members '[Reader CompilerCtx] r) => Asm.Tag -> Sem r ConstructorInfo
getConstructorInfo' tag = asks (^?! compilerConstructorInfos . at tag . _Just)

getConstructorMemRep :: (Members '[Compiler] r) => Asm.Tag -> Sem r NockmaMemRep
getConstructorMemRep tag = (^. constructorInfoMemRep) <$> getConstructorInfo tag

getConstructorArity :: (Members '[Compiler] r) => Asm.Tag -> Sem r Natural
getConstructorArity tag = (^. constructorInfoArity) <$> getConstructorInfo tag

re :: (Member (Reader CompilerCtx) r) => Sem (Compiler ': r) a -> Sem (Output (Term Natural) ': r) a
re = reinterpretH $ \case
  PushOnto s n -> pushOntoH s n
  PopFromN n s -> popFromNH n s
  PopNAndPushOnto s n t -> popNAndPushOnto' s n t >>= pureT
  Verbatim s -> outputT s
  TraceTerm s -> whenM (asks (^. compilerOptions . compilerOptionsEnableTrace)) (traceTerm' s) >>= pureT
  CallHelper isTail funName funArgsNum -> callHelper' isTail funName funArgsNum >>= pureT
  IncrementOn s -> incrementOn' s >>= pureT
  Branch t f -> branch' t f
  Save isTail m -> save' isTail m
  CallStdlibOn s f -> callStdlibOn' s f >>= pureT
  AsmReturn -> asmReturn' >>= pureT
  TestEqOn s -> testEqOn' s >>= pureT
  GetConstructorInfo s -> getConstructorInfo' s >>= pureT
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
pushOnStack sn t = OpPush # t # moveTopToStack
  where
    moveTopToStack :: Term Natural
    moveTopToStack =
      remakeList
        [ let p = OpAddress # (R : stackPath s)
           in if
                  | sn == s -> (OpAddress # indexStack 0) # p
                  | otherwise -> p
          | s <- allElements
        ]

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

replaceTopStackN :: Natural -> StackId -> Term Natural
replaceTopStackN n sn =
  remakeList
    [ let p = R : stackPath s
       in if
              | sn == s -> (OpAddress # indexStack 0) # (OpAddress # p ++ replicate n R)
              | otherwise -> OpAddress # p
      | s <- allElements
    ]

replaceTopStack :: StackId -> Term Natural
replaceTopStack = replaceTopStackN 1

pushNat :: (Member Compiler r) => Natural -> Sem r ()
pushNat = pushNatOnto ValueStack

pushNatOnto :: (Member Compiler r) => StackId -> Natural -> Sem r ()
pushNatOnto s n = pushOnto s (OpQuote # toNock n)

compileAndRunNock' ::
  (Members '[Reader EvalOptions, Output (Term Natural)] r) =>
  CompilerOptions ->
  ConstructorInfos ->
  [CompilerFunction] ->
  CompilerFunction ->
  Sem r (Term Natural)
compileAndRunNock' opts constrs funs mainfun =
  let Cell nockSubject t = runCompilerWith opts constrs funs mainfun
   in evalCompiledNock' nockSubject t

evalCompiledNock' :: (Members '[Reader EvalOptions, Output (Term Natural)] r) => Term Natural -> Term Natural -> Sem r (Term Natural)
evalCompiledNock' stack mainTerm = do
  evalT <-
    runError @(ErrNockNatural Natural)
      . runError @(NockEvalError Natural)
      $ eval stack mainTerm
  case evalT of
    Left e -> error (show e)
    Right ev -> case ev of
      Left e -> error (ppTrace e)
      Right res -> return res

-- | Used in testing and app
getStack :: StackId -> Term Natural -> Term Natural
getStack st m =
  fromRight'
    . run
    . runError @(NockEvalError Natural)
    . topEvalCtx
    . subTerm m
    $ stackPath st
