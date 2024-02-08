module Juvix.Compiler.Nockma.Translation.FromTree where

import Juvix.Compiler.Nockma.Language.Path
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Stdlib
import Juvix.Compiler.Nockma.StdlibFunction
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Tree.Data.InfoTable qualified as Tree
import Juvix.Compiler.Tree.Language qualified as Tree
import Juvix.Compiler.Tree.Language.Rep
import Juvix.Prelude hiding (Atom, Path)

nockmaMemRep :: MemRep -> NockmaMemRep
nockmaMemRep = \case
  MemRepTuple -> NockmaMemRepTuple
  MemRepConstr -> NockmaMemRepConstr
  MemRepTag -> NockmaMemRepConstr
  MemRepUnit -> NockmaMemRepConstr
  MemRepUnpacked {} -> NockmaMemRepConstr

data NockmaMemRepListConstr
  = NockmaMemRepListConstrNil
  | NockmaMemRepListConstrCons
  deriving stock (Eq)

data NockmaMemRep
  = NockmaMemRepConstr
  | NockmaMemRepTuple
  | NockmaMemRepList NockmaMemRepListConstr

newtype NockmaBuiltinTag
  = NockmaBuiltinBool Bool

type UserFunctionId = Symbol

data FunctionId
  = UserFunction UserFunctionId
  | BuiltinFunction BuiltinFunctionId
  deriving stock (Generic, Eq)

instance Hashable FunctionId

data BuiltinFunctionId
  = -- | Not intended to be used, it exists only so the Enum and Bounded instances can be derived.
    BuiltinPlaceholder
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

type ConstructorInfos = HashMap Tree.Tag ConstructorInfo

data CompilerFunction = CompilerFunction
  { _compilerFunctionName :: FunctionId,
    _compilerFunctionArity :: Natural,
    _compilerFunction :: Sem '[Reader CompilerCtx] (Term Natural)
  }

data StackId
  = Args
  | TempStack
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

data ConstructorPathId
  = ConstructorTag
  | ConstructorArgs
  deriving stock (Bounded, Enum)

constructorPath :: ConstructorPathId -> Path
constructorPath = pathFromEnum

data FunctionPathId
  = FunctionCode

functionPath :: FunctionPathId -> Path
functionPath = \case
  FunctionCode -> []

stackPath :: StackId -> Path
stackPath s = indexStack (fromIntegral (fromEnum s))

indexTuple :: Natural -> Natural -> Path
indexTuple len idx
  | idx >= len = impossible
  | otherwise =
      let lastL
            | idx == len - 1 = []
            | otherwise = [L]
       in replicate idx R ++ lastL

indexStack :: Natural -> Path
indexStack idx = replicate idx R ++ [L]

indexInStack :: StackId -> Natural -> Path
indexInStack s idx = stackPath s ++ indexStack idx

pathToArg :: Natural -> Path
pathToArg = indexInStack Args

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

makeFunction :: (FunctionPathId -> Term Natural) -> Term Natural
makeFunction f = f FunctionCode

foldTerms :: NonEmpty (Term Natural) -> Term Natural
foldTerms = foldr1 (#)

allConstructors :: Tree.InfoTable -> Tree.ConstructorInfo -> NonEmpty Tree.ConstructorInfo
allConstructors Tree.InfoTable {..} ci =
  let indInfo = getInductiveInfo (ci ^. Tree.constructorInductive)
   in nonEmpty' (getConstructorInfo'' <$> indInfo ^. Tree.inductiveConstructors)
  where
    getInductiveInfo :: Symbol -> Tree.InductiveInfo
    getInductiveInfo s = _infoInductives ^?! at s . _Just

    getConstructorInfo'' :: Tree.Tag -> Tree.ConstructorInfo
    getConstructorInfo'' t = _infoConstrs ^?! at t . _Just

supportsListNockmaRep :: Tree.InfoTable -> Tree.ConstructorInfo -> Maybe NockmaMemRepListConstr
supportsListNockmaRep tab ci = case allConstructors tab ci of
  c1 :| [c2]
    | [0, 2] `elem` permutations ((^. Tree.constructorArgsNum) <$> [c1, c2]) -> Just $ case ci ^. Tree.constructorArgsNum of
        0 -> NockmaMemRepListConstrNil
        2 -> NockmaMemRepListConstrCons
        _ -> impossible
    | otherwise -> Nothing
  _ -> Nothing

-- | Use `Tree.toNockma` before calling this function
fromTreeTable :: (Members '[Error JuvixError, Reader CompilerOptions] r) => Tree.InfoTable -> Sem r (Cell Natural)
fromTreeTable t = case t ^. Tree.infoMainFunction of
  Just mainFun -> do
    opts <- ask
    return (fromTree opts mainFun t)
  Nothing -> throw @JuvixError (error "TODO missing main")
  where
    fromTree :: CompilerOptions -> Tree.Symbol -> Tree.InfoTable -> Cell Natural
    fromTree opts mainSym tab@Tree.InfoTable {..} =
      let funs = map compileFunction allFunctions
          mkConstructorInfo :: Tree.ConstructorInfo -> ConstructorInfo
          mkConstructorInfo ci@Tree.ConstructorInfo {..} =
            ConstructorInfo
              { _constructorInfoArity = fromIntegral _constructorArgsNum,
                _constructorInfoMemRep = rep
              }
            where
              rep :: NockmaMemRep
              rep = maybe r NockmaMemRepList (supportsListNockmaRep tab ci)
                where
                  r = nockmaMemRep (memRep ci (getInductiveInfo (ci ^. Tree.constructorInductive)))

          constrs :: ConstructorInfos
          constrs = mkConstructorInfo <$> _infoConstrs

          getInductiveInfo :: Symbol -> Tree.InductiveInfo
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

        mainCode :: Tree.Node
        mainCode = _infoFunctions ^?! at mainSym . _Just . Tree.functionCode

        allFunctions :: [Tree.FunctionInfo]
        allFunctions = filter notMain (toList _infoFunctions)
          where
            notMain :: Tree.FunctionInfo -> Bool
            notMain Tree.FunctionInfo {..} = _functionSymbol /= mainSym

        compileFunction :: Tree.FunctionInfo -> CompilerFunction
        compileFunction Tree.FunctionInfo {..} =
          CompilerFunction
            { _compilerFunctionName = UserFunction _functionSymbol,
              _compilerFunctionArity = fromIntegral _functionArgsNum,
              _compilerFunction = compile _functionCode
            }

        memRep :: Tree.ConstructorInfo -> Tree.InductiveInfo -> Tree.MemRep
        memRep ci ind
          | numArgs >= 1 && numConstrs == 1 = MemRepTuple
          | otherwise = MemRepConstr
          where
            numConstrs = length (ind ^. Tree.inductiveConstructors)
            numArgs = ci ^. Tree.constructorArgsNum

fromOffsetRef :: Tree.OffsetRef -> Natural
fromOffsetRef = fromIntegral . (^. Tree.offsetRefOffset)

compile :: forall r. (Members '[Reader CompilerCtx] r) => Tree.Node -> Sem r (Term Natural)
compile = \case
  Tree.Binop b -> goBinop b
  Tree.Unop b -> goUnop b
  Tree.Const c -> return (goConst (c ^. Tree.nodeConstant))
  Tree.MemRef c -> goMemRef (c ^. Tree.nodeMemRef)
  Tree.AllocConstr c -> goAllocConstr c
  Tree.AllocClosure c -> goAllocClosure c
  Tree.ExtendClosure c -> goExtendClosure c
  Tree.Call c -> goCall c
  Tree.Branch b -> goBranch b
  Tree.Case c -> goCase c
  Tree.Save s -> goSave s
  Tree.CallClosures {} -> impossible
  where
    goAllocConstr :: Tree.NodeAllocConstr -> Sem r (Term Natural)
    goAllocConstr Tree.NodeAllocConstr {..} = do
      args <- mapM compile _nodeAllocConstrArgs
      info <- getConstructorInfo _nodeAllocConstrTag
      let memrep = info ^. constructorInfoMemRep
      return $ goConstructor memrep _nodeAllocConstrTag args

    goMemRef :: Tree.MemRef -> Sem r (Term Natural)
    goMemRef = \case
      Tree.DRef d -> return (goDirectRef d)
      Tree.ConstrRef Tree.Field {..} -> do
        info <- getConstructorInfo _fieldTag
        let memrep = info ^. constructorInfoMemRep
            argIx = fromIntegral _fieldOffset
            arity = info ^. constructorInfoArity
            path = case memrep of
              NockmaMemRepConstr ->
                directRefPath _fieldRef
                  ++ constructorPath ConstructorArgs
                  ++ indexStack argIx
              NockmaMemRepTuple ->
                directRefPath _fieldRef
                  ++ indexTuple arity argIx
              NockmaMemRepList constr -> case constr of
                NockmaMemRepListConstrNil -> impossible
                NockmaMemRepListConstrCons -> directRefPath _fieldRef ++ indexTuple 2 argIx
        return (OpAddress # path)
      where
        goDirectRef :: Tree.DirectRef -> Term Natural
        goDirectRef dr = OpAddress # directRefPath dr

    goConst :: Tree.Constant -> Term Natural
    goConst = \case
      Tree.ConstInt i
        | i < 0 -> error "negative integer"
        | otherwise -> nockIntegralLiteral i
      Tree.ConstBool i -> nockBoolLiteral i
      Tree.ConstString {} -> stringsErr
      Tree.ConstUnit -> OpQuote # constUnit
      Tree.ConstVoid -> OpQuote # constVoid

    goSave :: Tree.NodeSave -> Sem r (Term Natural)
    goSave Tree.NodeSave {..} = do
      arg <- compile _nodeSaveArg
      body <- compile _nodeSaveBody
      return (withTemp arg body)

    goCase :: Tree.NodeCase -> Sem r (Term Natural)
    goCase c = do
      def <- mapM compile (c ^. Tree.nodeCaseDefault)
      arg <- compile (c ^. Tree.nodeCaseArg)
      branches <-
        sequence
          [ do
              let withTemp' t
                    | b ^. Tree.caseBranchSave = withTemp arg t
                    | otherwise = t

              body' <- withTemp' <$> compile (b ^. Tree.caseBranchBody)
              return (b ^. Tree.caseBranchTag, body')
            | b <- c ^. Tree.nodeCaseBranches
          ]
      caseCmd arg def branches

    goBranch :: Tree.NodeBranch -> Sem r (Term Natural)
    goBranch Tree.NodeBranch {..} = do
      arg <- compile _nodeBranchArg
      iftrue <- compile _nodeBranchTrue
      iffalse <- compile _nodeBranchFalse
      return (branch arg iftrue iffalse)

    goUnop :: Tree.NodeUnop -> Sem r (Term Natural)
    goUnop Tree.NodeUnop {..} = do
      arg <- compile _nodeUnopArg
      return $ case _nodeUnopOpcode of
        Tree.OpShow -> stringsErr
        Tree.OpStrToInt -> stringsErr
        Tree.OpFail -> crash
        Tree.OpTrace -> OpTrace # arg # arg
        Tree.OpArgsNum ->
          let getF f = getClosureField f arg
           in sub (getF ClosureTotalArgsNum) (getF ClosureArgsNum)

    goBinop :: Tree.NodeBinop -> Sem r (Term Natural)
    goBinop Tree.NodeBinop {..} = do
      arg1 <- compile _nodeBinopArg1
      arg2 <- compile _nodeBinopArg2
      let args = [arg1, arg2]
      case _nodeBinopOpcode of
        Tree.IntAdd -> return (callStdlib StdlibAdd args)
        Tree.IntSub -> return (callStdlib StdlibSub args)
        Tree.IntMul -> return (callStdlib StdlibMul args)
        Tree.IntDiv -> return (callStdlib StdlibDiv args)
        Tree.IntMod -> return (callStdlib StdlibMod args)
        Tree.IntLt -> return (callStdlib StdlibLt args)
        Tree.IntLe -> return (callStdlib StdlibLe args)
        Tree.OpSeq -> return (OpHint # (nockNil' # arg1) # arg2)
        Tree.ValEq -> testEq _nodeBinopArg1 _nodeBinopArg2
        Tree.StrConcat -> stringsErr

    goAllocClosure :: Tree.NodeAllocClosure -> Sem r (Term Natural)
    goAllocClosure Tree.NodeAllocClosure {..} = do
      let fun = UserFunction _nodeAllocClosureFunSymbol
      fpath <- getFunctionPath fun
      farity <- getFunctionArity fun
      args <- mapM compile _nodeAllocClosureArgs
      return . makeClosure $ \case
        ClosureCode -> OpAddress # fpath
        ClosureTotalArgsNum -> nockNatLiteral farity
        ClosureArgsNum -> nockIntegralLiteral (length args)
        ClosureArgs -> remakeList args

    goExtendClosure :: Tree.NodeExtendClosure -> Sem r (Term Natural)
    goExtendClosure = extendClosure

    goCall :: Tree.NodeCall -> Sem r (Term Natural)
    goCall Tree.NodeCall {..} = do
      newargs <- mapM compile _nodeCallArgs
      case _nodeCallType of
        Tree.CallFun fun -> callFun (UserFunction fun) newargs
        Tree.CallClosure f -> do
          f' <- compile f
          let argsNum = getClosureField ClosureArgsNum f'
              oldArgs = getClosureField ClosureArgs f'
              fcode = getClosureField ClosureCode f'
              posOfArgsNil = appendRights emptyPath argsNum
              allArgs = replaceSubterm' oldArgs posOfArgsNil (remakeList newargs)
          return (OpApply # replaceArgsWithTerm allArgs # fcode)

appendRights :: Path -> Term Natural -> Term Natural
appendRights path n = dec (mul (pow2 n) (OpInc # OpQuote # path))

pushTemp :: Term Natural -> Term Natural
pushTemp toBePushed =
  remakeList
    [ let p = OpAddress # stackPath s
       in if
              | TempStack == s -> toBePushed # p
              | otherwise -> p
      | s <- allElements
    ]

withTemp :: Term Natural -> Term Natural -> Term Natural
withTemp toBePushed body =
  OpSequence # pushTemp toBePushed # body

testEq :: (Members '[Reader CompilerCtx] r) => Tree.Node -> Tree.Node -> Sem r (Term Natural)
testEq a b = do
  a' <- compile a
  b' <- compile b
  return (OpEq # a' # b')

nockNatLiteral :: Natural -> Term Natural
nockNatLiteral = nockIntegralLiteral

nockIntegralLiteral :: (Integral a) => a -> Term Natural
nockIntegralLiteral = (OpQuote #) . toNock @Natural . fromIntegral

extendClosure ::
  (Members '[Reader CompilerCtx] r) =>
  Tree.NodeExtendClosure ->
  Sem r (Term Natural)
extendClosure Tree.NodeExtendClosure {..} = do
  args <- mapM compile _nodeExtendClosureArgs
  closure <- compile _nodeExtendClosureFun
  let argsNum = getClosureField ClosureArgsNum closure
      oldArgs = getClosureField ClosureArgs closure
      fcode = getClosureField ClosureCode closure
      posOfArgsNil = appendRights emptyPath argsNum
      allArgs = replaceSubterm' oldArgs posOfArgsNil (remakeList args)
      newArgsNum = add argsNum (nockIntegralLiteral (length _nodeExtendClosureArgs))
  return . makeClosure $ \case
    ClosureCode -> fcode
    ClosureTotalArgsNum -> getClosureField ClosureTotalArgsNum closure
    ClosureArgsNum -> newArgsNum
    ClosureArgs -> allArgs

-- Calling convention for Anoma stdlib
--
-- [push
--   [seq [@ locStdlib] getF]     ::  Obtain the function f within the stdlib.
--                                ::       locStdlib is the location of the stdlib in the subject
--                                ::       getF is a term that fetches f relative to the stdlib
--   [call L                      ::  eval formula @ L of the following
--     [replace [RL               ::  edit at axis RL
--          [seq [@ R]            ::  evaluate the a formula in the original context without f on it
--           a]]                  ::  the formula giving a goes here
--      @ L]                      ::  this whole replace is editing what's at axis L, i.e. what was
--   ]
-- ]
callStdlib :: StdlibFunction -> [Term Natural] -> Term Natural
callStdlib fun args =
  let fPath = stdlibPath fun
      getFunCode = OpAddress # stackPath StandardLibrary >># fPath
      adjustArgs = case nonEmpty args of
        Just args' -> OpReplace # ([R, L] # ((OpAddress # [R]) >># foldTerms args')) # (OpAddress # [L])
        Nothing -> OpAddress # [L]
      callFn = OpCall # [L] # adjustArgs
      callCell = set cellCall (Just meta) (OpPush #. (getFunCode # callFn))
      meta =
        StdlibCall
          { _stdlibCallArgs = maybe nockNil' foldTerms (nonEmpty args),
            _stdlibCallFunction = fun
          }
   in TermCell callCell

constUnit :: Term Natural
constUnit = constVoid

constVoid :: Term Natural
constVoid = TermAtom nockVoid

directRefPath :: Tree.DirectRef -> Path
directRefPath = \case
  Tree.ArgRef a -> pathToArg (fromOffsetRef a)
  Tree.TempRef Tree.RefTemp {..} ->
    tempRefPath
      (fromIntegral (fromJust _refTempTempHeight))
      (fromOffsetRef _refTempOffsetRef)

tempRefPath :: Natural -> Natural -> Path
tempRefPath tempHeight off = indexInStack TempStack (tempHeight - off - 1)

nockmaBuiltinTag :: Tree.BuiltinDataTag -> NockmaBuiltinTag
nockmaBuiltinTag = \case
  Tree.TagTrue -> NockmaBuiltinBool True
  Tree.TagFalse -> NockmaBuiltinBool False
  Tree.TagReturn -> impossible
  Tree.TagBind -> impossible
  Tree.TagWrite -> impossible
  Tree.TagReadLn -> impossible

-- | Generic constructors are encoded as [tag args], where args is a
-- nil terminated list.
goConstructor :: NockmaMemRep -> Tree.Tag -> [Term Natural] -> Term Natural
goConstructor mr t args = case t of
  Tree.BuiltinTag b -> case nockmaBuiltinTag b of
    NockmaBuiltinBool v -> nockBoolLiteral v
  Tree.UserTag tag -> case mr of
    NockmaMemRepConstr ->
      makeConstructor $ \case
        ConstructorTag -> OpQuote # (fromIntegral (tag ^. Tree.tagUserWord) :: Natural)
        ConstructorArgs -> remakeList args
    NockmaMemRepTuple -> foldTerms (nonEmpty' args)
    NockmaMemRepList constr -> case constr of
      NockmaMemRepListConstrNil
        | null args -> remakeList []
        | otherwise -> impossible
      NockmaMemRepListConstrCons -> case args of
        [l, r] -> TCell l r
        _ -> impossible

unsupported :: Text -> a
unsupported thing = error ("The Nockma backend does not support " <> thing)

stringsErr :: a
stringsErr = unsupported "strings"

-- | Computes a - b
sub :: Term Natural -> Term Natural -> Term Natural
sub a b = callStdlib StdlibSub [a, b]

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
      Args -> nockNil'
      TempStack -> nockNil'
      StandardLibrary -> stdlib
      FunctionsLibrary -> makeList defs

runCompilerWith :: CompilerOptions -> ConstructorInfos -> [CompilerFunction] -> CompilerFunction -> Cell Natural
runCompilerWith opts constrs libFuns mainFun =
  let entryCommand :: (Members '[Reader CompilerCtx] r) => Sem r (Term Natural)
      entryCommand = callFun (mainFun ^. compilerFunctionName) []

      entryTerm =
        run
          . runReader compilerCtx
          $ entryCommand

      compiledFuns :: NonEmpty (Term Natural)
      compiledFuns =
        makeFunction'
          <$> ( run . runReader compilerCtx . (^. compilerFunction)
                  <$> allFuns
              )

      makeFunction' :: Term Natural -> Term Natural
      makeFunction' c = makeFunction $ \case
        FunctionCode -> c

      ret = Cell (initStack (toList compiledFuns)) entryTerm
   in ret
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
  BuiltinPlaceholder ->
    CompilerFunction
      { _compilerFunctionName = BuiltinFunction BuiltinPlaceholder,
        _compilerFunctionArity = 0,
        _compilerFunction = return crash
      }

callFun ::
  (Members '[Reader CompilerCtx] r) =>
  FunctionId ->
  [Term Natural] ->
  Sem r (Term Natural)
callFun fun args = do
  fpath <- getFunctionPath fun
  let p' = fpath ++ functionPath FunctionCode
  return (OpCall # p' # replaceArgs args)

replaceArgsWithTerm :: Term Natural -> Term Natural
replaceArgsWithTerm term =
  remakeList
    [ if
          | Args == s -> term
          | otherwise -> OpAddress # stackPath s
      | s <- allElements
    ]

replaceArgs :: [Term Natural] -> Term Natural
replaceArgs args =
  remakeList
    [ if
          | Args == s -> remakeList args
          | otherwise -> OpAddress # stackPath s
      | s <- allElements
    ]

getFunctionPath :: (Members '[Reader CompilerCtx] r) => FunctionId -> Sem r Path
getFunctionPath funName = asks (^?! compilerFunctionInfos . at funName . _Just . functionInfoPath)

evaluated :: Term Natural -> Term Natural
evaluated t = OpApply # (OpAddress # emptyPath) # t

-- | obj[eval(relPath)] := newVal
-- relPath is relative to obj
replaceSubterm' :: Term Natural -> Term Natural -> Term Natural -> Term Natural
replaceSubterm' obj relPath newVal =
  evaluated $ (OpQuote # OpReplace) # ((relPath # (OpQuote # newVal)) # (OpQuote # obj))

builtinTagToTerm :: NockmaBuiltinTag -> Term Natural
builtinTagToTerm = \case
  NockmaBuiltinBool v -> nockBoolLiteral v

constructorTagToTerm :: Tree.Tag -> Term Natural
constructorTagToTerm = \case
  Tree.UserTag t -> OpQuote # toNock (fromIntegral (t ^. Tree.tagUserWord) :: Natural)
  Tree.BuiltinTag b -> builtinTagToTerm (nockmaBuiltinTag b)

caseCmd ::
  forall r.
  (Members '[Reader CompilerCtx] r) =>
  Term Natural ->
  Maybe (Term Natural) ->
  [(Tree.Tag, Term Natural)] ->
  Sem r (Term Natural)
caseCmd arg defaultBranch = \case
  [] -> return (fromJust defaultBranch)
  (tag, b) : bs -> case tag of
    Tree.BuiltinTag t -> case nockmaBuiltinTag t of
      NockmaBuiltinBool v -> return (goBoolTag v b bs)
    Tree.UserTag {} -> do
      rep <- getConstructorMemRep tag
      case rep of
        NockmaMemRepConstr -> goRepConstr tag b bs
        NockmaMemRepTuple
          | null bs, isNothing defaultBranch -> return b
          | otherwise -> error "redundant branch. Impossible?"
        NockmaMemRepList constr -> do
          bs' <- mapM (firstM asNockmaMemRepListConstr) bs
          return (goRepList ((constr, b) :| bs'))
  where
    goRepConstr ::
      Tree.Tag ->
      Term Natural ->
      [(Tree.Tag, Term Natural)] ->
      Sem r (Term Natural)
    goRepConstr tag b bs = do
      let cond :: Term Natural =
            OpEq
              # constructorTagToTerm tag
              # (getConstructorField ConstructorTag arg)
      elseBr <- caseCmd arg defaultBranch bs
      return (branch cond b elseBr)

    asNockmaMemRepListConstr :: Tree.Tag -> Sem r NockmaMemRepListConstr
    asNockmaMemRepListConstr tag = case tag of
      Tree.UserTag {} -> do
        rep <- getConstructorMemRep tag
        case rep of
          NockmaMemRepList constr -> return constr
          _ -> impossible
      Tree.BuiltinTag {} -> impossible

    goBoolTag ::
      Bool ->
      Term Natural ->
      [(Tree.Tag, Term Natural)] ->
      (Term Natural)
    goBoolTag v b bs =
      let otherBranch = fromMaybe crash (firstJust f bs <|> defaultBranch)
       in if
              | v -> branch arg b otherBranch
              | otherwise -> branch arg otherBranch b
      where
        f :: (Tree.Tag, Term Natural) -> Maybe (Term Natural)
        f (tag', br) = case tag' of
          Tree.UserTag {} -> impossible
          Tree.BuiltinTag tag -> case nockmaBuiltinTag tag of
            NockmaBuiltinBool v' -> guard (v /= v') $> br

    goRepList :: NonEmpty (NockmaMemRepListConstr, Term Natural) -> Term Natural
    goRepList ((c, b) :| bs) =
      let cond = OpIsCell # arg
          otherBranch = fromMaybe crash (firstJust f bs <|> defaultBranch)
       in case c of
            NockmaMemRepListConstrCons -> branch cond b otherBranch
            NockmaMemRepListConstrNil -> branch cond otherBranch b
      where
        f :: (NockmaMemRepListConstr, Term Natural) -> Maybe (Term Natural)
        f (c', br) = guard (c /= c') $> br

branch ::
  Term Natural ->
  Term Natural ->
  Term Natural ->
  Term Natural
branch cond t f = OpIf # cond # t # f

getFunctionArity :: (Members '[Reader CompilerCtx] r) => FunctionId -> Sem r Natural
getFunctionArity s = asks (^?! compilerFunctionInfos . at s . _Just . functionInfoArity)

getConstructorInfo :: (Members '[Reader CompilerCtx] r) => Tree.Tag -> Sem r ConstructorInfo
getConstructorInfo tag = asks (^?! compilerConstructorInfos . at tag . _Just)

getClosureField :: ClosurePathId -> Term Natural -> Term Natural
getClosureField = getField

getConstructorField :: ConstructorPathId -> Term Natural -> Term Natural
getConstructorField = getField

getField :: (Enum field) => field -> Term Natural -> Term Natural
getField field t = t >># (OpAddress # pathFromEnum field)

getConstructorMemRep :: (Members '[Reader CompilerCtx] r) => Tree.Tag -> Sem r NockmaMemRep
getConstructorMemRep tag = (^. constructorInfoMemRep) <$> getConstructorInfo tag

crash :: Term Natural
crash = (OpAddress # OpAddress # OpAddress)

mul :: Term Natural -> Term Natural -> Term Natural
mul a b = callStdlib StdlibMul [a, b]

pow2 :: Term Natural -> Term Natural
pow2 = callStdlib StdlibPow2 . pure

add :: Term Natural -> Term Natural -> Term Natural
add a b = callStdlib StdlibAdd [a, b]

dec :: Term Natural -> Term Natural
dec = callStdlib StdlibDec . pure
