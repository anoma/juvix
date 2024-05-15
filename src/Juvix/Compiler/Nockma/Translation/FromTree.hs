module Juvix.Compiler.Nockma.Translation.FromTree
  ( fromEntryPoint,
    fromTreeTable,
    AnomaResult (..),
    anomaClosure,
    compilerFunctionId,
    compilerFunctionName,
    AnomaCallablePathId (..),
    CompilerOptions (..),
    CompilerFunction (..),
    FunctionCtx (..),
    FunctionId (..),
    closurePath,
    foldTermsOrNil,
    add,
    dec,
    mul,
    sub,
    pow2,
    nockNatLiteral,
    nockIntegralLiteral,
    callStdlib,
    appendRights,
    foldTerms,
    pathToArg,
    makeList,
    listToTuple,
    appendToTuple,
    append,
    opAddress',
    replaceSubterm',
    runCompilerWith,
  )
where

import Juvix.Compiler.Nockma.Language.Path
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Stdlib
import Juvix.Compiler.Nockma.StdlibFunction
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Tree.Data.InfoTable qualified as Tree
import Juvix.Compiler.Tree.Language qualified as Tree
import Juvix.Compiler.Tree.Language.Rep
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude hiding (Atom, Path)

newtype AnomaResult = AnomaResult
  { _anomaClosure :: Term Natural
  }

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
    _functionInfoArity :: Natural,
    _functionInfoName :: Text
  }

newtype FunctionCtx = FunctionCtx
  { _functionCtxArity :: Natural
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
  { _compilerFunctionId :: FunctionId,
    _compilerFunctionName :: Text,
    _compilerFunctionArity :: Natural,
    _compilerFunction :: Sem '[Reader CompilerCtx, Reader FunctionCtx] (Term Natural)
  }

-- The Code and Args constructors must be first and second respectively. This is
-- because the stack must have the structure of a Nock function,
-- i.e [code args env]
data AnomaCallablePathId
  = WrapperCode
  | ArgsTuple
  | FunctionsLibrary
  | RawCode
  | TempStack
  | StandardLibrary
  | ClosureTotalArgsNum
  | ClosureArgsNum
  | ClosureArgs
  | AnomaGetOrder
  deriving stock (Enum, Bounded, Eq, Show)

-- | A closure has the following structure:
-- [code totalArgsNum argsNum args], where
-- 1. code is code to run when fully applied.
-- 2. totalArgsNum is the number of arguments that the function
--     which created the closure expects.
-- 3. argsNum is the number of arguments that have been applied to the closure.
-- 4. args is the list of args that have been applied.
--    The length of the list should be argsNum.
pathFromEnum :: (Enum a) => a -> Path
pathFromEnum = indexStack . fromIntegral . fromEnum

data ConstructorPathId
  = ConstructorTag
  | ConstructorArgs
  deriving stock (Bounded, Enum)

constructorPath :: ConstructorPathId -> Path
constructorPath = pathFromEnum

stackPath :: AnomaCallablePathId -> Path
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

indexInStack :: AnomaCallablePathId -> Natural -> Path
indexInStack s idx = stackPath s ++ indexStack idx

makeLenses ''CompilerOptions
makeLenses ''AnomaResult
makeLenses ''CompilerFunction
makeLenses ''CompilerCtx
makeLenses ''FunctionCtx
makeLenses ''ConstructorInfo
makeLenses ''FunctionInfo

runCompilerFunction :: CompilerCtx -> CompilerFunction -> Term Natural
runCompilerFunction ctx fun =
  run
    . runReader (FunctionCtx (fun ^. compilerFunctionArity))
    . runReader ctx
    $ fun ^. compilerFunction

pathToArg :: (Members '[Reader FunctionCtx] r) => Natural -> Sem r Path
pathToArg n = do
  ari <- asks (^. functionCtxArity)
  return (stackPath ArgsTuple <> indexTuple ari n)

termFromParts :: (Bounded p, Enum p) => (p -> Term Natural) -> Term Natural
termFromParts f = remakeList [f pi | pi <- allElements]

makeClosure :: (AnomaCallablePathId -> Term Natural) -> Term Natural
makeClosure = termFromParts

makeConstructor :: (ConstructorPathId -> Term Natural) -> Term Natural
makeConstructor = termFromParts

foldTermsOrNil :: [Term Natural] -> Term Natural
foldTermsOrNil = maybe (OpQuote # nockNilTagged "foldTermsOrNil") foldTerms . nonEmpty

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
fromTreeTable :: (Members '[Error JuvixError, Reader CompilerOptions] r) => Tree.InfoTable -> Sem r AnomaResult
fromTreeTable t = case t ^. Tree.infoMainFunction of
  Just mainFun -> do
    opts <- ask
    return (fromTree opts mainFun t)
  Nothing -> throw @JuvixError (error "TODO missing main")
  where
    fromTree :: CompilerOptions -> Tree.Symbol -> Tree.InfoTable -> AnomaResult
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
        mainFun = compileFunction (_infoFunctions ^?! at mainSym . _Just)

        allFunctions :: [Tree.FunctionInfo]
        allFunctions = filter notMain (toList _infoFunctions)
          where
            notMain :: Tree.FunctionInfo -> Bool
            notMain Tree.FunctionInfo {..} = _functionSymbol /= mainSym

        compileFunction :: Tree.FunctionInfo -> CompilerFunction
        compileFunction Tree.FunctionInfo {..} =
          CompilerFunction
            { _compilerFunctionId = UserFunction _functionSymbol,
              _compilerFunctionName = _functionName,
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

anomaCallableClosureWrapper :: Term Natural
anomaCallableClosureWrapper =
  let closureArgsNum :: Term Natural = getClosureFieldInSubject ClosureArgsNum
      closureTotalArgsNum :: Term Natural = getClosureFieldInSubject ClosureTotalArgsNum
      appendAndReplaceArgsTuple =
        replaceArgsWithTerm "anomaCallableClosureWrapper" $
          appendToTuple
            (getClosureFieldInSubject ClosureArgs)
            closureArgsNum
            (getClosureFieldInSubject ArgsTuple)
            (sub closureTotalArgsNum closureArgsNum)
      closureArgsIsEmpty = isZero closureArgsNum
      adjustArgs = OpIf # closureArgsIsEmpty # (opAddress "wrapperSubject" emptyPath) # appendAndReplaceArgsTuple
   in opCall "closureWrapper" (closurePath RawCode) adjustArgs

mainFunctionWrapper :: Term Natural
mainFunctionWrapper =
  -- 1. The Anoma system expects to receive a function of type `ScryId -> Transaction`
  --
  -- 2. The ScryId is only used to construct the argument to the Scry operation (i.e the anomaGet builtin in the Juvix frontend),
  --
  -- 3. When the Juvix developer writes a function to submit to Anoma they use
  -- type `() -> Transaction`, this wrapper is used to capture the ScryId
  -- argument into the subject which is then used to construct OpScry arguments
  -- when anomaGet is compiled.
  --
  -- 4. If the Anoma system expectation changes then this code must be changed.
  let captureAnomaGetOrder :: Term Natural
      captureAnomaGetOrder = replaceSubject $ \case
        AnomaGetOrder -> Just (getClosureFieldInSubject ArgsTuple)
        _ -> Nothing
   in opCall "mainFunctionWrapper" (closurePath RawCode) captureAnomaGetOrder

compile :: forall r. (Members '[Reader FunctionCtx, Reader CompilerCtx] r) => Tree.Node -> Sem r (Term Natural)
compile = \case
  Tree.Binop b -> goBinop b
  Tree.Unop b -> goUnop b
  Tree.Cairo {} -> cairoErr
  Tree.Constant c -> return (goConstant (c ^. Tree.nodeConstant))
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
      Tree.DRef d -> goDirectRef d
      Tree.ConstrRef Tree.Field {..} -> do
        info <- getConstructorInfo _fieldTag
        let memrep = info ^. constructorInfoMemRep
            argIx = fromIntegral _fieldOffset
            arity = info ^. constructorInfoArity
            path :: Sem r Path
            path = do
              fr <- directRefPath _fieldRef
              return $ case memrep of
                NockmaMemRepConstr ->
                  fr
                    ++ constructorPath ConstructorArgs
                    ++ indexStack argIx
                NockmaMemRepTuple ->
                  fr
                    ++ indexTuple arity argIx
                NockmaMemRepList constr -> case constr of
                  NockmaMemRepListConstrNil -> impossible
                  NockmaMemRepListConstrCons -> fr ++ indexTuple 2 argIx
        (opAddress "constrRef") <$> path
      where
        goDirectRef :: Tree.DirectRef -> Sem r (Term Natural)
        goDirectRef dr = do
          p <- directRefPath dr
          return (opAddress "directRef" p)

    goConstant :: Tree.Constant -> Term Natural
    goConstant = \case
      Tree.ConstInt i
        | i < 0 -> error "negative integer"
        | otherwise -> nockIntegralLiteral i
      Tree.ConstBool i -> nockBoolLiteral i
      Tree.ConstString {} -> stringsErr
      Tree.ConstUnit -> OpQuote # constUnit
      Tree.ConstVoid -> OpQuote # constVoid
      Tree.ConstField {} -> fieldErr

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
      case _nodeUnopOpcode of
        Tree.PrimUnop op -> return $ goPrimUnop op arg
        Tree.OpFail -> return crash
        Tree.OpTrace -> goTrace arg
        Tree.OpAnomaGet -> goAnomaGet arg
        Tree.OpAnomaEncode -> goAnomaEncode arg

    goPrimUnop :: Tree.UnaryOp -> Term Natural -> Term Natural
    goPrimUnop op arg = case op of
      Tree.OpShow -> stringsErr
      Tree.OpStrToInt -> stringsErr
      Tree.OpArgsNum ->
        let getF f = getClosureField f arg
         in sub (getF ClosureTotalArgsNum) (getF ClosureArgsNum)
      Tree.OpIntToField -> fieldErr
      Tree.OpFieldToInt -> fieldErr

    goAnomaGet :: Term Natural -> Sem r (Term Natural)
    goAnomaGet key = do
      let arg = remakeList [getFieldInSubject AnomaGetOrder, key]
      return (OpScry # (OpQuote # nockNilTagged "OpScry-typehint") # arg)

    goAnomaEncode :: Term Natural -> Sem r (Term Natural)
    goAnomaEncode arg = return (callStdlib StdlibEncode [arg])

    goTrace :: Term Natural -> Sem r (Term Natural)
    goTrace arg = do
      enabled <- asks (^. compilerOptions . compilerOptionsEnableTrace)
      return $
        if
            | enabled -> OpTrace # arg # arg
            | otherwise -> arg

    goBinop :: Tree.NodeBinop -> Sem r (Term Natural)
    goBinop Tree.NodeBinop {..} = do
      arg1 <- compile _nodeBinopArg1
      arg2 <- compile _nodeBinopArg2
      case _nodeBinopOpcode of
        Tree.PrimBinop op -> goPrimBinop op [arg1, arg2]
        Tree.OpSeq -> return (OpHint # (nockNilTagged "OpSeq-OpHint-annotation" # arg1) # arg2)
      where
        goPrimBinop :: Tree.BinaryOp -> [Term Natural] -> Sem r (Term Natural)
        goPrimBinop op args = case op of
          Tree.OpIntAdd -> return (callStdlib StdlibAdd args)
          Tree.OpIntSub -> return (callStdlib StdlibSub args)
          Tree.OpIntMul -> return (callStdlib StdlibMul args)
          Tree.OpIntDiv -> return (callStdlib StdlibDiv args)
          Tree.OpIntMod -> return (callStdlib StdlibMod args)
          Tree.OpIntLt -> return (callStdlib StdlibLt args)
          Tree.OpIntLe -> return (callStdlib StdlibLe args)
          Tree.OpEq -> testEq _nodeBinopArg1 _nodeBinopArg2
          Tree.OpStrConcat -> stringsErr
          Tree.OpFieldAdd -> fieldErr
          Tree.OpFieldSub -> fieldErr
          Tree.OpFieldMul -> fieldErr
          Tree.OpFieldDiv -> fieldErr

    goAllocClosure :: Tree.NodeAllocClosure -> Sem r (Term Natural)
    goAllocClosure Tree.NodeAllocClosure {..} = do
      let fun = UserFunction _nodeAllocClosureFunSymbol
      fpath <- getFunctionPath fun
      farity <- getFunctionArity fun
      args <- mapM compile _nodeAllocClosureArgs
      return . makeClosure $ \case
        WrapperCode -> OpQuote # anomaCallableClosureWrapper
        ArgsTuple -> OpQuote # argsTuplePlaceholder "goAllocClosure"
        FunctionsLibrary -> OpQuote # functionsLibraryPlaceHolder
        RawCode -> opAddress "allocClosureFunPath" (fpath <> closurePath RawCode)
        TempStack -> remakeList []
        StandardLibrary -> OpQuote # stdlib
        ClosureTotalArgsNum -> nockNatLiteral farity
        ClosureArgsNum -> nockIntegralLiteral (length args)
        ClosureArgs -> remakeList args
        AnomaGetOrder -> OpQuote # nockNilTagged "goAllocClosure-AnomaGetOrder"

    goExtendClosure :: Tree.NodeExtendClosure -> Sem r (Term Natural)
    goExtendClosure = extendClosure

    goCall :: Tree.NodeCall -> Sem r (Term Natural)
    goCall Tree.NodeCall {..} = do
      newargs <- mapM compile _nodeCallArgs
      case _nodeCallType of
        Tree.CallFun fun -> callFunWithArgs (UserFunction fun) newargs
        Tree.CallClosure f -> do
          closure <- compile f
          let argsNum = getClosureField ClosureArgsNum closure
              oldArgs = getClosureField ClosureArgs closure
              allArgs = appendToTuple oldArgs argsNum (foldTermsOrNil newargs) (nockIntegralLiteral (length newargs))
              newSubject = replaceSubject $ \case
                WrapperCode -> Just (getClosureField RawCode closure) -- We Want RawCode because we already have all args.
                ArgsTuple -> Just allArgs
                RawCode -> Just (OpQuote # nockNilTagged "callClosure-RawCode")
                TempStack -> Just (OpQuote # nockNilTagged "callClosure-TempStack")
                FunctionsLibrary -> Nothing
                StandardLibrary -> Nothing
                ClosureArgs -> Nothing
                ClosureTotalArgsNum -> Nothing
                ClosureArgsNum -> Nothing
                AnomaGetOrder -> Nothing
          return (opCall "callClosure" (closurePath WrapperCode) newSubject)

isZero :: Term Natural -> Term Natural
isZero a = OpEq # a # nockNatLiteral 0

opAddress' :: Term Natural -> Term Natural
opAddress' x = evaluated $ (opQuote "opAddress'" OpAddress) # x

-- [a [b [c 0]]] -> [a [b c]]
-- len = quote 3
-- TODO lst is being evaluated three times!
listToTuple :: Term Natural -> Term Natural -> Term Natural
listToTuple lst len =
  -- posOfLast uses stdlib so when it is evaulated the stdlib must be in the
  -- subject lst must also be evaluated against the standard subject. We achieve
  -- this by evaluating `lst #. posOfLastOffset` in `t1`. The address that
  -- posOfLastOffset now points to must be shifted by [L] to make it relative to
  -- `lst`.
  let posOfLastOffset = appendRights [L] (dec len)
      posOfLast = appendRights emptyPath (dec len)
      t1 = (lst #. posOfLastOffset) >># (opAddress' (OpAddress # [R])) >># (opAddress "listToTupleLast" [L])
   in OpIf # isZero len # lst # (replaceSubterm' lst posOfLast t1)

argsTuplePlaceholder :: Text -> Term Natural
argsTuplePlaceholder txt = nockNilTagged ("argsTuplePlaceholder-" <> txt)

appendRights :: Path -> Term Natural -> Term Natural
appendRights path n = dec (mul (pow2 n) (OpInc # OpQuote # path))

withTemp :: Term Natural -> Term Natural -> Term Natural
withTemp toBePushed body =
  OpSequence # pushTemp # body
  where
    pushTemp :: Term Natural
    pushTemp =
      remakeList
        [ let p = opAddress "pushTemp" (stackPath s)
           in if
                  | TempStack == s -> toBePushed # p
                  | otherwise -> p
          | s <- allElements
        ]

testEq :: (Members '[Reader FunctionCtx, Reader CompilerCtx] r) => Tree.Node -> Tree.Node -> Sem r (Term Natural)
testEq a b = do
  a' <- compile a
  b' <- compile b
  return (OpEq # a' # b')

nockNatLiteral :: Natural -> Term Natural
nockNatLiteral = nockIntegralLiteral

nockIntegralLiteral :: (Integral a) => a -> Term Natural
nockIntegralLiteral = (OpQuote #) . toNock @Natural . fromIntegral

-- | xs must be a list.
-- ys is a (possibly empty) tuple.
-- the result is a tuple.
appendToTuple :: Term Natural -> Term Natural -> Term Natural -> Term Natural -> Term Natural
appendToTuple xs lenXs ys lenYs =
  OpIf # isZero lenYs # listToTuple xs lenXs # append xs lenXs ys

append :: Term Natural -> Term Natural -> Term Natural -> Term Natural
append xs lenXs ys =
  let posOfXsNil = appendRights emptyPath lenXs
   in replaceSubterm' xs posOfXsNil ys

extendClosure ::
  (Members '[Reader FunctionCtx, Reader CompilerCtx] r) =>
  Tree.NodeExtendClosure ->
  Sem r (Term Natural)
extendClosure Tree.NodeExtendClosure {..} = do
  args <- mapM compile _nodeExtendClosureArgs
  closure <- compile _nodeExtendClosureFun
  let argsNum = getClosureField ClosureArgsNum closure
      oldArgs = getClosureField ClosureArgs closure
      allArgs = append oldArgs argsNum (remakeList args)
      newArgsNum = add argsNum (nockIntegralLiteral (length _nodeExtendClosureArgs))
  return . makeClosure $ \case
    WrapperCode -> getClosureField WrapperCode closure
    RawCode -> getClosureField RawCode closure
    ClosureTotalArgsNum -> getClosureField ClosureTotalArgsNum closure
    ClosureArgsNum -> newArgsNum
    ClosureArgs -> allArgs
    ArgsTuple -> getClosureField ArgsTuple closure
    FunctionsLibrary -> getClosureField FunctionsLibrary closure
    TempStack -> getClosureField TempStack closure
    StandardLibrary -> getClosureField StandardLibrary closure
    AnomaGetOrder -> getClosureField AnomaGetOrder closure

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
--      @ L]                      ::  this whole replace is editing what's at axis L, i.e. what was pushed
--   ]
-- ]
callStdlib :: StdlibFunction -> [Term Natural] -> Term Natural
callStdlib fun args =
  let fPath = stdlibPath fun
      getFunCode = opAddress "callStdlibFunCode" (stackPath StandardLibrary) >># fPath
      adjustArgs = case nonEmpty args of
        Just args' -> opReplace "callStdlib-args" (closurePath ArgsTuple) ((opAddress "stdlibR" [R]) >># foldTerms args') (opAddress "stdlibL" [L])
        Nothing -> opAddress "adjustArgsNothing" [L]
      callFn = opCall "callStdlib" (closurePath WrapperCode) adjustArgs
      callCell = set cellCall (Just meta) (OpPush #. (getFunCode # callFn))
      meta =
        StdlibCall
          { _stdlibCallArgs = foldTermsOrNil args,
            _stdlibCallFunction = fun
          }
   in TermCell callCell

constUnit :: Term Natural
constUnit = constVoid

constVoid :: Term Natural
constVoid = TermAtom nockVoid

directRefPath :: forall r. (Members '[Reader FunctionCtx] r) => Tree.DirectRef -> Sem r Path
directRefPath = \case
  Tree.ArgRef a -> pathToArg (fromOffsetRef a)
  Tree.TempRef Tree.RefTemp {..} ->
    return
      ( tempRefPath
          (fromIntegral (fromJust _refTempTempHeight))
          (fromOffsetRef _refTempOffsetRef)
      )

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

fieldErr :: a
fieldErr = unsupported "the field type"

cairoErr :: a
cairoErr = unsupported "cairo builtins"

-- | Computes a - b
sub :: Term Natural -> Term Natural -> Term Natural
sub a b = callStdlib StdlibSub [a, b]

makeList :: (Foldable f) => f (Term Natural) -> Term Natural
makeList ts = foldTerms (toList ts `prependList` pure (nockNilTagged "makeList"))

remakeList :: (Foldable l) => l (Term Natural) -> Term Natural
remakeList ts = foldTerms (toList ts `prependList` pure (OpQuote # nockNilTagged "remakeList"))

runCompilerWith :: CompilerOptions -> ConstructorInfos -> [CompilerFunction] -> CompilerFunction -> AnomaResult
runCompilerWith opts constrs moduleFuns mainFun = makeAnomaFun
  where
    libFuns :: [CompilerFunction]
    libFuns = moduleFuns ++ (builtinFunction <$> allElements)

    allFuns :: NonEmpty CompilerFunction
    allFuns = mainFun :| libFuns

    compilerCtx :: CompilerCtx
    compilerCtx =
      CompilerCtx
        { _compilerFunctionInfos = functionInfos,
          _compilerConstructorInfos = constrs,
          _compilerOptions = opts
        }

    mainClosure :: Term Natural
    mainClosure = makeMainFunction (runCompilerFunction compilerCtx mainFun)

    compiledFuns :: NonEmpty (Term Natural)
    compiledFuns =
      mainClosure
        :| ( makeLibraryFunction
               <$> [(f ^. compilerFunctionName, runCompilerFunction compilerCtx f) | f <- libFuns]
           )

    funcsLib :: Term Natural
    funcsLib = Str.theFunctionsLibrary @ makeList compiledFuns

    makeLibraryFunction :: (Text, Term Natural) -> Term Natural
    makeLibraryFunction (funName, c) =
      ("def-" <> funName)
        @ ( makeClosure $ \p ->
              let nockNilHere = nockNilTagged ("makeLibraryFunction-" <> show p)
               in case p of
                    WrapperCode -> ("wrapperCode-" <> funName) @ c
                    ArgsTuple -> ("argsTuple-" <> funName) @ argsTuplePlaceholder "libraryFunction"
                    FunctionsLibrary -> ("functionsLibrary-" <> funName) @ functionsLibraryPlaceHolder
                    RawCode -> ("rawCode-" <> funName) @ c
                    TempStack -> ("tempStack-" <> funName) @ nockNilHere
                    StandardLibrary -> ("stdlib-" <> funName) @ stdlib
                    ClosureTotalArgsNum -> ("closureTotalArgsNum-" <> funName) @ nockNilHere
                    ClosureArgsNum -> ("closureArgsNum-" <> funName) @ nockNilHere
                    ClosureArgs -> ("closureArgs-" <> funName) @ nockNilHere
                    AnomaGetOrder -> ("anomaGetOrder-" <> funName) @ nockNilHere
          )

    makeMainFunction :: Term Natural -> Term Natural
    makeMainFunction c = makeClosure $ \p ->
      let nockNilHere = nockNilTagged ("makeMainFunction-" <> show p)
       in case p of
            WrapperCode -> mainFunctionWrapper
            ArgsTuple -> argsTuplePlaceholder "mainFunction"
            FunctionsLibrary -> functionsLibraryPlaceHolder
            RawCode -> c
            TempStack -> nockNilHere
            StandardLibrary -> stdlib
            ClosureTotalArgsNum -> nockNilHere
            ClosureArgsNum -> nockNilHere
            ClosureArgs -> nockNilHere
            AnomaGetOrder -> nockNilHere

    functionInfos :: HashMap FunctionId FunctionInfo
    functionInfos = hashMap (run (runStreamOfNaturals (toList <$> userFunctions)))

    userFunctions :: (Members '[StreamOf Natural] r) => Sem r (NonEmpty (FunctionId, FunctionInfo))
    userFunctions = forM allFuns $ \CompilerFunction {..} -> do
      i <- yield
      return
        ( _compilerFunctionId,
          FunctionInfo
            { _functionInfoPath = indexInStack FunctionsLibrary i,
              _functionInfoArity = _compilerFunctionArity,
              _functionInfoName = _compilerFunctionName
            }
        )

    -- Replaces all instances of functionsLibraryPlaceHolder by the actual
    -- functions library. Note that the functions library will have
    -- functionsLibraryPlaceHolders, but this is not an issue because they
    -- are not directly accessible from anoma so they'll never be entrypoints.
    substFuncsLib :: Term Natural -> Term Natural
    substFuncsLib = \case
      TermAtom a
        | a ^. atomHint == Just AtomHintFunctionsPlaceholder -> funcsLib
        | otherwise -> TermAtom a
      TermCell (Cell' l r i) ->
        -- note that we do not need to recurse into terms inside the CellInfo because those terms will never be an entry point from anoma
        TermCell (Cell' (substFuncsLib l) (substFuncsLib r) i)

    makeAnomaFun :: AnomaResult
    makeAnomaFun =
      AnomaResult
        { _anomaClosure = substFuncsLib (substFuncsLib mainClosure)
        }

functionsLibraryPlaceHolder :: Term Natural
functionsLibraryPlaceHolder =
  TermAtom
    Atom
      { _atomInfo =
          AtomInfo
            { _atomInfoLoc = Irrelevant Nothing,
              _atomInfoTag = Nothing,
              _atomInfoHint = Just AtomHintFunctionsPlaceholder
            },
        _atom = 0 :: Natural
      }

builtinFunction :: BuiltinFunctionId -> CompilerFunction
builtinFunction = \case
  BuiltinPlaceholder ->
    CompilerFunction
      { _compilerFunctionId = BuiltinFunction BuiltinPlaceholder,
        _compilerFunctionArity = 0,
        _compilerFunction = return crash,
        _compilerFunctionName = "builtinPlaceholderName"
      }

closurePath :: AnomaCallablePathId -> Path
closurePath = stackPath

-- | Call a function. Arguments to the function are assumed to be in the ArgsTuple stack
-- TODO what about temporary stack?
callFun ::
  (Members '[Reader CompilerCtx] r) =>
  FunctionId ->
  Sem r (Term Natural)
callFun fun = do
  fpath <- getFunctionPath fun
  fname <- getFunctionName fun
  let p' = fpath ++ closurePath WrapperCode
  return (opCall ("callFun-" <> fname) p' (opAddress "callFunSubject" emptyPath))

-- | Call a function with the passed arguments
callFunWithArgs ::
  (Members '[Reader CompilerCtx] r) =>
  FunctionId ->
  [Term Natural] ->
  Sem r (Term Natural)
callFunWithArgs fun args = (replaceArgs args >>#) <$> callFun fun

replaceSubject :: (AnomaCallablePathId -> Maybe (Term Natural)) -> Term Natural
replaceSubject = replaceSubject' "replaceSubject"

replaceSubject' :: Text -> (AnomaCallablePathId -> Maybe (Term Natural)) -> Term Natural
replaceSubject' tag f =
  remakeList
    [ case f s of
        Nothing -> opAddress tag (closurePath s)
        Just t' -> t'
      | s <- allElements
    ]

replaceArgsWithTerm :: Text -> Term Natural -> Term Natural
replaceArgsWithTerm tag term =
  replaceSubject' ("replaceArgsWithTerm-" <> tag) $ \case
    ArgsTuple -> Just term
    _ -> Nothing

replaceArgs :: [Term Natural] -> Term Natural
replaceArgs = replaceArgsWithTerm "replaceArgs" . foldTermsOrNil

getFunctionInfo :: (Members '[Reader CompilerCtx] r) => FunctionId -> Sem r FunctionInfo
getFunctionInfo funId = asks (^?! compilerFunctionInfos . at funId . _Just)

getFunctionPath :: (Members '[Reader CompilerCtx] r) => FunctionId -> Sem r Path
getFunctionPath funId = (^. functionInfoPath) <$> getFunctionInfo funId

getFunctionName :: (Members '[Reader CompilerCtx] r) => FunctionId -> Sem r Text
getFunctionName funId = (^. functionInfoName) <$> getFunctionInfo funId

evaluated :: Term Natural -> Term Natural
evaluated t = OpApply # (opAddress "evaluated" emptyPath) # t

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
      case nonEmpty bs of
        Nothing -> case defaultBranch of
          Nothing -> return b
          Just defbr -> return (branch cond b defbr)
        Just ((t', b') :| bs') -> do
          elseBr <- goRepConstr t' b' bs'
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

getClosureField :: AnomaCallablePathId -> Term Natural -> Term Natural
getClosureField = getField

getClosureFieldInSubject :: AnomaCallablePathId -> Term Natural
getClosureFieldInSubject = getFieldInSubject

getConstructorField :: ConstructorPathId -> Term Natural -> Term Natural
getConstructorField = getField

getField :: (Enum field) => field -> Term Natural -> Term Natural
getField field t = t >># getFieldInSubject field

getFieldInSubject :: (Enum field) => field -> Term Natural
getFieldInSubject field = opAddress "getFieldInSubject" (pathFromEnum field)

getConstructorMemRep :: (Members '[Reader CompilerCtx] r) => Tree.Tag -> Sem r NockmaMemRep
getConstructorMemRep tag = (^. constructorInfoMemRep) <$> getConstructorInfo tag

crash :: Term Natural
crash = ("crash" @ OpAddress # OpAddress # OpAddress)

mul :: Term Natural -> Term Natural -> Term Natural
mul a b = callStdlib StdlibMul [a, b]

pow2 :: Term Natural -> Term Natural
pow2 = callStdlib StdlibPow2 . pure

add :: Term Natural -> Term Natural -> Term Natural
add a b = callStdlib StdlibAdd [a, b]

dec :: Term Natural -> Term Natural
dec = callStdlib StdlibDec . pure
