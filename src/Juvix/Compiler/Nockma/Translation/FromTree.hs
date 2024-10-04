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
    sub,
    dec,
    mul,
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
    emptyCompilerCtx,
    CompilerCtx (..),
  )
where

import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Encoding.Ed25519 qualified as E
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

data NockmaMemRepMaybeConstr
  = NockmaMemRepMaybeConstrNothing
  | NockmaMemRepMaybeConstrJust
  deriving stock (Eq)

data NockmaMemRep
  = NockmaMemRepConstr
  | NockmaMemRepTuple
  | NockmaMemRepList NockmaMemRepListConstr
  | NockmaMemRepMaybe NockmaMemRepMaybeConstr

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

newtype TempRef = TempRef
  { _tempRefIndex :: Int
  }

data CompilerCtx = CompilerCtx
  { _compilerFunctionInfos :: HashMap FunctionId FunctionInfo,
    _compilerConstructorInfos :: ConstructorInfos,
    -- | Maps temporary variables to their stack indices.
    _compilerTempVarMap :: HashMap Int TempRef,
    _compilerTempVarsNum :: Int,
    _compilerStackHeight :: Int,
    _compilerOptions :: CompilerOptions
  }

emptyCompilerCtx :: CompilerCtx
emptyCompilerCtx =
  CompilerCtx
    { _compilerFunctionInfos = mempty,
      _compilerConstructorInfos = mempty,
      _compilerTempVarMap = mempty,
      _compilerTempVarsNum = 0,
      _compilerStackHeight = 0,
      _compilerOptions = CompilerOptions True
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

-- | The Code and Args constructors must be first and second respectively. This is
-- because the stack must have the structure of a Nock function,
-- i.e [code args env]
data AnomaCallablePathId
  = FunCode
  | ArgsTuple
  | ---
    FunctionsLibrary
  | StandardLibrary
  | ClosureTotalArgsNum
  | ClosureArgsNum
  | ClosureArgs
  | AnomaGetOrder
  deriving stock (Enum, Bounded, Eq, Show)

indexStack :: Natural -> Path
indexStack idx = replicate idx R ++ [L]

pathFromEnum :: (Enum a) => a -> Path
pathFromEnum = indexStack . fromIntegral . fromEnum

data ConstructorPathId
  = ConstructorTag
  | ConstructorArgs
  deriving stock (Bounded, Enum)

constructorPath :: ConstructorPathId -> Path
constructorPath = pathFromEnum

closurePath :: AnomaCallablePathId -> Path
closurePath = pathFromEnum

data IndexTupleArgs = IndexTupleArgs
  { _indexTupleArgsLength :: Natural,
    _indexTupleArgsIndex :: Natural
  }

indexTuple :: IndexTupleArgs -> Path
indexTuple IndexTupleArgs {..}
  | _indexTupleArgsIndex >= _indexTupleArgsLength = impossible
  | otherwise =
      let lastL
            | _indexTupleArgsIndex == _indexTupleArgsLength - 1 = []
            | otherwise = [L]
       in replicate _indexTupleArgsIndex R ++ lastL

makeLenses ''CompilerOptions
makeLenses ''AnomaResult
makeLenses ''CompilerFunction
makeLenses ''CompilerCtx
makeLenses ''FunctionCtx
makeLenses ''ConstructorInfo
makeLenses ''FunctionInfo

stackPath :: (Member (Reader CompilerCtx) r, Enum field) => field -> Sem r Path
stackPath s = do
  h <- asks (^. compilerStackHeight)
  return $ indexStack (fromIntegral (h + fromEnum s))

getSubjectBasePath :: (Member (Reader CompilerCtx) r) => Sem r Path
getSubjectBasePath = do
  h <- asks (^. compilerStackHeight)
  return $ replicate h R

-- | Pushes a temporary value onto the subject stack and continues compilation
-- with the provided continuation function.
--
-- NOTE: It is *important* to *never* duplicate any compilation steps, e.g.,
-- ```
-- doSth <- compile something
-- return $ doSth # doSth
-- ```
-- is incorrect. Duplication of `doSth` in the returned generated code may
-- result in changing the asymptotic complexity of the compiled program
-- exponentially. The above code should be replaced with:
-- ```
-- doSth <- compile something
-- withTemp doSth $ \ref -> do
--   val <- addressTempRef ref
--   return $ val # val
withTemp ::
  (Member (Reader CompilerCtx) r) =>
  Term Natural ->
  (TempRef -> Sem r (Term Natural)) ->
  Sem r (Term Natural)
withTemp value f = do
  stackHeight <- asks (^. compilerStackHeight)
  body' <- local (over compilerStackHeight (+ 1)) $ f (TempRef stackHeight)
  return $ OpPush # value # body'

withTempVar ::
  (Member (Reader CompilerCtx) r) =>
  Term Natural ->
  (TempRef -> Sem r (Term Natural)) ->
  Sem r (Term Natural)
withTempVar value cont = withTemp value $ \temp -> do
  tempVar <- asks (^. compilerTempVarsNum)
  local (over compilerTempVarMap (HashMap.insert tempVar temp))
    . local (over compilerTempVarsNum (+ 1))
    $ cont temp

popTempVar ::
  (Member (Reader CompilerCtx) r) =>
  (Sem r (Term Natural)) ->
  Sem r (Term Natural)
popTempVar cont = do
  tempVar <- asks (^. compilerTempVarsNum)
  local (over compilerTempVarMap (HashMap.delete (tempVar - 1)))
    . local (over compilerTempVarsNum (\x -> x - 1))
    $ cont

runCompilerFunction :: CompilerCtx -> CompilerFunction -> Term Natural
runCompilerFunction ctx fun =
  run
    . runReader (FunctionCtx (fun ^. compilerFunctionArity))
    . runReader ctx
    $ fun ^. compilerFunction

pathToArg :: (Members '[Reader FunctionCtx, Reader CompilerCtx] r) => Natural -> Sem r Path
pathToArg n = do
  ari <- asks (^. functionCtxArity)
  path <- stackPath ArgsTuple
  return
    ( path
        <> indexTuple
          IndexTupleArgs
            { _indexTupleArgsLength = ari,
              _indexTupleArgsIndex = n
            }
    )

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

supportsListNockmaRep :: Tree.InfoTable -> Tree.ConstructorInfo -> Maybe NockmaMemRep
supportsListNockmaRep tab ci =
  NockmaMemRepList <$> case allConstructors tab ci of
    c1 :| [c2]
      | [0, 2] `elem` permutations ((^. Tree.constructorArgsNum) <$> [c1, c2]) -> Just $ case ci ^. Tree.constructorArgsNum of
          0 -> NockmaMemRepListConstrNil
          2 -> NockmaMemRepListConstrCons
          _ -> impossible
      | otherwise -> Nothing
    _ -> Nothing

supportsMaybeNockmaRep :: Tree.InfoTable -> Tree.ConstructorInfo -> Maybe NockmaMemRep
supportsMaybeNockmaRep tab ci =
  NockmaMemRepMaybe <$> case allConstructors tab ci of
    c1 :| [c2]
      | [0, 1] `elem` permutations ((^. Tree.constructorArgsNum) <$> [c1, c2]) -> Just $ case ci ^. Tree.constructorArgsNum of
          0 -> NockmaMemRepMaybeConstrNothing
          1 -> NockmaMemRepMaybeConstrJust
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
              rep = fromMaybe r (supportsListNockmaRep tab ci <|> supportsMaybeNockmaRep tab ci)
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

tempRefPath :: (Member (Reader CompilerCtx) r) => TempRef -> Sem r Path
tempRefPath (TempRef idx) = do
  h <- asks (^. compilerStackHeight)
  return $ indexStack (fromIntegral (h - idx - 1))

addressTempRef :: (Member (Reader CompilerCtx) r) => TempRef -> Sem r (Term Natural)
addressTempRef tr = do
  p <- tempRefPath tr
  return $ opAddress "tempRef" p

mainFunctionWrapper :: (Member (Reader CompilerCtx) r) => Term Natural -> Term Natural -> Sem r (Term Natural)
mainFunctionWrapper funslib funCode = do
  -- 1. The Anoma system expects to receive a function of type `ScryId -> Transaction`
  --
  -- 2. The ScryId is only used to construct the argument to the Scry operation
  --    (i.e the anomaGet builtin in the Juvix frontend),
  --
  -- 3. When the Juvix developer writes a function to submit to Anoma they use
  -- type `() -> Transaction`, this wrapper is used to capture the ScryId
  -- argument into the subject which is then used to construct OpScry arguments
  -- when anomaGet is compiled.
  --
  -- 4. If the Anoma system expectation changes then this code must be changed.
  anomaGet <- getFieldInSubject ArgsTuple
  captureAnomaGetOrder <- replaceSubject $ \case
    FunCode -> Just (OpQuote # funCode)
    AnomaGetOrder -> Just anomaGet
    FunctionsLibrary -> Just (OpQuote # funslib)
    _ -> Nothing
  return $ opCall "mainFunctionWrapper" (closurePath FunCode) captureAnomaGetOrder

compile :: forall r. (Members '[Reader FunctionCtx, Reader CompilerCtx] r) => Tree.Node -> Sem r (Term Natural)
compile = \case
  Tree.Binop b -> goBinop b
  Tree.Unop b -> goUnop b
  Tree.ByteArray b -> goByteArrayOp b
  Tree.Cairo {} -> cairoErr
  Tree.Anoma b -> goAnomaOp b
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
                    ++ indexTuple
                      IndexTupleArgs
                        { _indexTupleArgsLength = arity,
                          _indexTupleArgsIndex = argIx
                        }
                NockmaMemRepList constr -> case constr of
                  NockmaMemRepListConstrNil -> impossible
                  NockmaMemRepListConstrCons ->
                    fr
                      ++ indexTuple
                        IndexTupleArgs
                          { _indexTupleArgsLength = 2,
                            _indexTupleArgsIndex = argIx
                          }
                NockmaMemRepMaybe constr -> case constr of
                  NockmaMemRepMaybeConstrNothing -> impossible
                  -- just x is represented as [nil x] so argument index is offset by 1.
                  -- argIx will always be 0 because just has one argument
                  NockmaMemRepMaybeConstrJust ->
                    fr
                      ++ indexTuple
                        IndexTupleArgs
                          { _indexTupleArgsLength = 2,
                            _indexTupleArgsIndex = argIx + 1
                          }
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
      Tree.ConstString t -> OpQuote # goConstString t
      Tree.ConstUnit -> OpQuote # constUnit
      Tree.ConstVoid -> OpQuote # constVoid
      Tree.ConstField {} -> fieldErr
      Tree.ConstUInt8 i -> nockIntegralLiteral i
      Tree.ConstByteArray bs -> OpQuote # (toNock @Natural (fromIntegral (BS.length bs)) # toNock (byteStringToNatural bs))

    goConstString :: Text -> Term Natural
    goConstString t =
      TermAtom
        Atom
          { _atomInfo = atomHintInfo AtomHintString,
            _atom = textToNatural t
          }

    goSave :: Tree.NodeSave -> Sem r (Term Natural)
    goSave Tree.NodeSave {..} = do
      arg <- compile _nodeSaveArg
      withTempVar arg (const (compile _nodeSaveBody))

    goCase :: Tree.NodeCase -> Sem r (Term Natural)
    goCase c = do
      arg <- compile (c ^. Tree.nodeCaseArg)
      withTempVar arg $ \ref -> do
        def <- mapM (popTempVar . compile) (c ^. Tree.nodeCaseDefault)
        branches <- mapM goCaseBranch (c ^. Tree.nodeCaseBranches)
        caseCmd ref def branches

    goCaseBranch :: Tree.CaseBranch -> Sem r (Tree.Tag, Term Natural)
    goCaseBranch b = do
      let withSave t
            | b ^. Tree.caseBranchSave = t
            | otherwise = popTempVar t
      body' <- withSave $ compile (b ^. Tree.caseBranchBody)
      return (b ^. Tree.caseBranchTag, body')

    goBranch :: Tree.NodeBranch -> Sem r (Term Natural)
    goBranch Tree.NodeBranch {..} = do
      arg <- compile _nodeBranchArg
      iftrue <- compile _nodeBranchTrue
      iffalse <- compile _nodeBranchFalse
      return (branch arg iftrue iffalse)

    goAnomaOp :: Tree.NodeAnoma -> Sem r (Term Natural)
    goAnomaOp Tree.NodeAnoma {..} = do
      args <- mapM compile _nodeAnomaArgs
      case _nodeAnomaOpcode of
        Tree.OpAnomaGet -> goAnomaGet args
        Tree.OpAnomaEncode -> goAnomaEncode args
        Tree.OpAnomaDecode -> goAnomaDecode args
        Tree.OpAnomaVerifyDetached -> goAnomaVerifyDetached args
        Tree.OpAnomaSign -> goAnomaSign args
        Tree.OpAnomaVerifyWithMessage -> goAnomaVerifyWithMessage args
        Tree.OpAnomaSignDetached -> goAnomaSignDetached args
        Tree.OpAnomaByteArrayFromAnomaContents -> return (goAnomaByteArrayFromAnomaContents args)
        Tree.OpAnomaByteArrayToAnomaContents -> return (goAnomaByteArrayToAnomaContents args)

    goByteArrayOp :: Tree.NodeByteArray -> Sem r (Term Natural)
    goByteArrayOp Tree.NodeByteArray {..} = do
      args <- mapM compile _nodeByteArrayArgs
      case _nodeByteArrayOpcode of
        Tree.OpByteArrayLength -> return $ goByteArrayLength args
        Tree.OpByteArrayFromListUInt8 -> do
          len <- callStdlib StdlibLengthList args
          args' <- callStdlib StdlibFoldBytes args
          return $ len # args'
      where
        goByteArrayLength :: [Term Natural] -> Term Natural
        goByteArrayLength = \case
          [ba] -> ba >># opAddress "head-of-the-bytestring" [L]
          _ -> impossible

    goUnop :: Tree.NodeUnop -> Sem r (Term Natural)
    goUnop Tree.NodeUnop {..} =
      case _nodeUnopOpcode of
        Tree.PrimUnop op -> goPrimUnop op _nodeUnopArg
        Tree.OpAssert -> do
          arg <- compile _nodeUnopArg
          withTemp arg $ \ref -> do
            tmp <- addressTempRef ref
            return (branch tmp tmp crash)
        Tree.OpFail -> return crash
        Tree.OpTrace -> do
          arg <- compile _nodeUnopArg
          goTrace arg

    goPrimUnop :: Tree.UnaryOp -> Tree.Node -> Sem r (Term Natural)
    goPrimUnop op arg = case op of
      Tree.OpShow -> stringsErr "show"
      Tree.OpStrToInt -> stringsErr "strToInt"
      Tree.OpArgsNum -> do
        arg' <- compile arg
        withTemp
          arg'
          ( \ref -> do
              tmp <- addressTempRef ref
              sub (getClosureField ClosureTotalArgsNum tmp) (getClosureField ClosureArgsNum tmp)
          )
      Tree.OpIntToField -> fieldErr
      Tree.OpFieldToInt -> fieldErr
      Tree.OpIntToUInt8 -> intToUInt8 =<< compile arg
      Tree.OpUInt8ToInt -> compile arg

    goAnomaGet :: [Term Natural] -> Sem r (Term Natural)
    goAnomaGet key = do
      anomaGet <- getFieldInSubject AnomaGetOrder
      let arg = remakeList [anomaGet, foldTermsOrNil key]
      return (OpScry # (OpQuote # nockNilTagged "OpScry-typehint") # arg)

    goAnomaEncode :: [Term Natural] -> Sem r (Term Natural)
    goAnomaEncode = callStdlib StdlibEncode

    goAnomaDecode :: [Term Natural] -> Sem r (Term Natural)
    goAnomaDecode = callStdlib StdlibDecode

    byteArrayPayload :: Text -> Term Natural -> Term Natural
    byteArrayPayload msg ba = ba >># opAddress msg [R]

    mkByteArray :: Term Natural -> Term Natural -> Term Natural
    mkByteArray len payload = len # payload

    goAnomaVerifyDetached :: [Term Natural] -> Sem r (Term Natural)
    goAnomaVerifyDetached = \case
      [sig, message, pubKey] -> do
        enc <- goAnomaEncode [message]
        callStdlib
          StdlibVerifyDetached
          [ byteArrayPayload "verifyDetachedSig" sig,
            enc,
            byteArrayPayload "verifyDetachedPubKey" pubKey
          ]
      _ -> impossible

    goAnomaSign :: [Term Natural] -> Sem r (Term Natural)
    goAnomaSign = \case
      [message, privKey] -> do
        enc <- goAnomaEncode [message]
        stdcall <-
          callStdlib
            StdlibSign
            [ enc,
              byteArrayPayload "anomaSignPrivKeyTail" privKey
            ]
        withTemp stdcall goReturnByteArray
      _ -> impossible
      where
        goReturnByteArray :: TempRef -> Sem r (Term Natural)
        goReturnByteArray ref = do
          signResult <- addressTempRef ref
          res <- callStdlib StdlibLengthBytes [signResult]
          return $ mkByteArray res signResult

    goAnomaSignDetached :: [Term Natural] -> Sem r (Term Natural)
    goAnomaSignDetached = \case
      [message, privKeyByteArray] -> do
        enc <- goAnomaEncode [message]
        stdcall <-
          callStdlib
            StdlibSignDetached
            [ enc,
              byteArrayPayload "privKeyByteArrayTail" privKeyByteArray
            ]
        return $
          mkByteArray
            (nockNatLiteral (integerToNatural (toInteger E.signatureLength)))
            stdcall
      _ -> impossible

    goAnomaByteArrayToAnomaContents :: [Term Natural] -> Term Natural
    goAnomaByteArrayToAnomaContents = \case
      [ba] -> byteArrayPayload "bytearryToAnomaContents-payload" ba
      _ -> impossible

    goAnomaByteArrayFromAnomaContents :: [Term Natural] -> Term Natural
    goAnomaByteArrayFromAnomaContents = \case
      [len, contents] -> mkByteArray len contents
      _ -> impossible

    -- Conceptually this function is:
    -- anomaDecode <$> verify signedMessage pubKey
    --
    -- verify returns a `Maybe Nat` that is `Just msg` if the signedMessage is verified.
    goAnomaVerifyWithMessage :: [Term Natural] -> Sem r (Term Natural)
    goAnomaVerifyWithMessage = \case
      [signedMessage, pubKey] -> do
        stdcall <- callStdlib StdlibVerify [byteArrayPayload "signedMessageByteArray" signedMessage, byteArrayPayload "pubKeyByteArray" pubKey]
        withTemp stdcall goDecodeResult
      _ -> impossible
      where
        goDecodeResult :: TempRef -> Sem r (Term Natural)
        goDecodeResult ref = do
          decJust <- goDecodeResultJust ref
          res <- addressTempRef ref
          return $
            branch (OpIsCell # res) decJust res

        goDecodeResultJust :: TempRef -> Sem r (Term Natural)
        goDecodeResultJust ref = do
          refPath <- tempRefPath ref
          stdcall <- callStdlib StdlibDecode [opAddress "verify-result-just" (refPath ++ justPayloadPath)]
          return $
            opReplace
              "putDecodeResultInJust"
              justPayloadPath
              stdcall
              (opAddress "tempRef" refPath)
          where
            -- just x is represented as [nil x] so the payload of just is always at index 1.
            justPayloadPath :: Path
            justPayloadPath =
              indexTuple
                IndexTupleArgs
                  { _indexTupleArgsLength = 2,
                    _indexTupleArgsIndex = 1
                  }

    goTrace :: Term Natural -> Sem r (Term Natural)
    goTrace arg = do
      enabled <- asks (^. compilerOptions . compilerOptionsEnableTrace)
      if
          | enabled ->
              withTemp arg $ \ref -> do
                val <- addressTempRef ref
                return $ OpTrace # val # val
          | otherwise -> return arg

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
          Tree.OpIntAdd -> callStdlib StdlibAdd args
          Tree.OpIntSub -> callStdlib StdlibSub args
          Tree.OpIntMul -> callStdlib StdlibMul args
          Tree.OpIntDiv -> callStdlib StdlibDiv args
          Tree.OpIntMod -> callStdlib StdlibMod args
          Tree.OpBool Tree.OpIntLt -> callStdlib StdlibLt args
          Tree.OpBool Tree.OpIntLe -> callStdlib StdlibLe args
          Tree.OpBool Tree.OpEq -> testEq _nodeBinopArg1 _nodeBinopArg2
          Tree.OpStrConcat -> callStdlib StdlibCatBytes args
          Tree.OpFieldAdd -> fieldErr
          Tree.OpFieldSub -> fieldErr
          Tree.OpFieldMul -> fieldErr
          Tree.OpFieldDiv -> fieldErr

    goAllocClosure :: Tree.NodeAllocClosure -> Sem r (Term Natural)
    goAllocClosure Tree.NodeAllocClosure {..} = do
      let fun = UserFunction _nodeAllocClosureFunSymbol
      base <- getSubjectBasePath
      fpath <- getFunctionPath fun
      farity <- getFunctionArity fun
      args <- mapM compile _nodeAllocClosureArgs
      return . makeClosure $ \case
        FunCode -> opAddress "allocClosureFunPath" (base <> fpath <> closurePath FunCode)
        ArgsTuple -> OpQuote # argsTuplePlaceholder "goAllocClosure"
        FunctionsLibrary -> OpQuote # functionsLibraryPlaceHolder
        StandardLibrary -> OpQuote # stdlibPlaceHolder
        ClosureTotalArgsNum -> nockNatLiteral farity
        ClosureArgsNum -> nockIntegralLiteral (length args)
        ClosureArgs -> remakeList args
        AnomaGetOrder -> OpQuote # nockNilTagged "goAllocClosure-AnomaGetOrder"

    goExtendClosure :: Tree.NodeExtendClosure -> Sem r (Term Natural)
    goExtendClosure = extendClosure

    goCall :: Tree.NodeCall -> Sem r (Term Natural)
    goCall Tree.NodeCall {..} =
      case _nodeCallType of
        Tree.CallFun fun -> do
          newargs <- mapM compile _nodeCallArgs
          callFunWithArgs (UserFunction fun) newargs
        Tree.CallClosure f -> do
          closure <- compile f
          withTemp closure $ \ref -> do
            newargs <- mapM compile _nodeCallArgs
            callClosure ref newargs

isZero :: Term Natural -> Term Natural
isZero a = OpEq # a # nockNatLiteral 0

opAddress' :: Term Natural -> Term Natural
opAddress' x = evaluated $ (opQuote "opAddress'" OpAddress) # x

-- [a [b [c 0]]] -> [a [b c]]
-- len = quote 3
listToTuple :: (Member (Reader CompilerCtx) r) => Term Natural -> Term Natural -> Sem r (Term Natural)
listToTuple lst len = do
  -- posOfLast uses stdlib so when it is evaulated the stdlib must be in the
  -- subject lst must also be evaluated against the standard subject. We achieve
  -- this by evaluating `lst #. posOfLastOffset` in `t1`. The address that
  -- posOfLastOffset now points to must be shifted by [L] to make it relative to
  -- `lst`.
  --
  -- TODO: dec and the pow2 in appendRights are being evaluated twice. We should
  -- have appendRights' which takes 2^n instead of n
  --
  -- TODO: there is way too much arithmetic here with many calls to stdlib; this
  -- makes the generated code very inefficient
  withTemp lst $ \lstRef ->
    withTemp len $ \lenRef -> do
      lstVal <- addressTempRef lstRef
      lenVal <- addressTempRef lenRef
      posOfLastOffset <- appendRights [L] =<< dec lenVal
      posOfLast <- appendRights emptyPath =<< dec lenVal
      let t1 = (lstVal #. posOfLastOffset) >># (opAddress' (OpAddress # [R])) >># (opAddress "listToTupleLast" [L])
      return $
        OpIf # isZero lenVal # lstVal # (replaceSubterm' lstVal posOfLast t1)

argsTuplePlaceholder :: Text -> Term Natural
argsTuplePlaceholder txt = nockNilTagged ("argsTuplePlaceholder-" <> txt)

appendRights :: (Member (Reader CompilerCtx) r) => Path -> Term Natural -> Sem r (Term Natural)
appendRights path n = do
  n' <- pow2 n
  mul n' (OpInc # OpQuote # path) >>= dec

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
-- NOTE: xs occurs twice, but that's fine because each occurrence is in a
-- different if branch.
-- TODO: this function generates extremely inefficient code
appendToTuple ::
  (Member (Reader CompilerCtx) r) =>
  Term Natural ->
  Term Natural ->
  Term Natural ->
  Term Natural ->
  Sem r (Term Natural)
appendToTuple xs lenXs ys lenYs = do
  tp1 <- listToTuple xs lenXs
  tp2 <- append xs lenXs ys
  -- TODO: omit the if when lenYs is known at compile-time
  return $ OpIf # isZero lenYs # tp1 # tp2

-- TODO: what does this function do? what are the arguments?
-- TODO: this function generates inefficient code
append :: (Member (Reader CompilerCtx) r) => Term Natural -> Term Natural -> Term Natural -> Sem r (Term Natural)
append xs lenXs ys = do
  posOfXsNil <- appendRights emptyPath lenXs
  return $ replaceSubterm' xs posOfXsNil ys

extendClosure ::
  (Members '[Reader FunctionCtx, Reader CompilerCtx] r) =>
  Tree.NodeExtendClosure ->
  Sem r (Term Natural)
extendClosure Tree.NodeExtendClosure {..} = do
  closureFun <- compile _nodeExtendClosureFun
  withTemp closureFun $ \ref -> do
    args <- mapM compile _nodeExtendClosureArgs
    closure <- addressTempRef ref
    let argsNum = getClosureField ClosureArgsNum closure
        oldArgs = getClosureField ClosureArgs closure
    allArgs <- append oldArgs argsNum (remakeList args)
    newArgsNum <- add argsNum (nockIntegralLiteral (length _nodeExtendClosureArgs))
    return . makeClosure $ \case
      FunCode -> getClosureField FunCode closure
      ClosureTotalArgsNum -> getClosureField ClosureTotalArgsNum closure
      ClosureArgsNum -> newArgsNum
      ClosureArgs -> allArgs
      ArgsTuple -> getClosureField ArgsTuple closure
      FunctionsLibrary -> getClosureField FunctionsLibrary closure
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
callStdlib :: (Member (Reader CompilerCtx) r) => StdlibFunction -> [Term Natural] -> Sem r (Term Natural)
callStdlib fun args = do
  stdpath <- stackPath StandardLibrary
  let fPath = stdlibPath fun
      getFunCode = opAddress "callStdlibFunCode" stdpath >># fPath
  argsPath <- stackPath ArgsTuple
  let adjustArgs = case nonEmpty args of
        Just args' -> opReplace "callStdlib-args" argsPath ((opAddress "stdlibR" [R]) >># foldTerms args') (opAddress "stdlibL" [L])
        Nothing -> opAddress "adjustArgsNothing" [L]
      callFn = opCall "callStdlib" (closurePath FunCode) adjustArgs
      meta =
        StdlibCall
          { _stdlibCallArgs = foldTermsOrNil args,
            _stdlibCallFunction = fun
          }
      callCell = set cellCall (Just meta) (OpPush #. (getFunCode # callFn))
   in return $ TermCell callCell

constUnit :: Term Natural
constUnit = constVoid

constVoid :: Term Natural
constVoid = TermAtom nockVoid

directRefPath :: forall r. (Members '[Reader FunctionCtx, Reader CompilerCtx] r) => Tree.DirectRef -> Sem r Path
directRefPath = \case
  Tree.ArgRef a -> pathToArg (fromOffsetRef a)
  Tree.TempRef Tree.RefTemp {..} -> do
    varMap <- asks (^. compilerTempVarMap)
    let tempIdx = _refTempOffsetRef ^. Tree.offsetRefOffset
        ref = fromJust $ HashMap.lookup tempIdx varMap
    tempRefPath ref

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
    NockmaMemRepMaybe constr -> case constr of
      NockmaMemRepMaybeConstrNothing
        | null args -> (OpQuote # nockNilTagged "maybe-constr-nothing")
        | otherwise -> impossible
      NockmaMemRepMaybeConstrJust -> case args of
        [x] -> TCell (OpQuote # nockNilTagged "maybe-constr-just-head") x
        _ -> impossible

unsupported :: Text -> a
unsupported thing = error ("The Nockma backend does not support " <> thing)

stringsErr :: Text -> a
stringsErr t = unsupported ("strings: " <> t)

fieldErr :: a
fieldErr = unsupported "the field type"

cairoErr :: a
cairoErr = unsupported "cairo builtins"

makeList :: (Foldable f) => f (Term Natural) -> Term Natural
makeList ts = foldTerms (toList ts `prependList` pure (nockNilTagged "makeList"))

remakeList :: (Foldable l) => l (Term Natural) -> Term Natural
remakeList ts = foldTerms (toList ts `prependList` pure (OpQuote # nockNilTagged "remakeList"))

runCompilerWith :: CompilerOptions -> ConstructorInfos -> [CompilerFunction] -> CompilerFunction -> AnomaResult
runCompilerWith opts constrs moduleFuns mainFun =
  AnomaResult
    { _anomaClosure = mainClosure
    }
  where
    libFuns :: [CompilerFunction]
    libFuns = moduleFuns ++ (builtinFunction <$> allElements)

    allFuns :: NonEmpty CompilerFunction
    allFuns = mainFun :| libFuns

    compilerCtx :: CompilerCtx
    compilerCtx =
      emptyCompilerCtx
        { _compilerFunctionInfos = functionInfos,
          _compilerConstructorInfos = constrs,
          _compilerOptions = opts
        }

    mainClosure :: Term Natural
    mainClosure = makeMainFunction (runCompilerFunction compilerCtx mainFun)

    funcsLib :: Term Natural
    funcsLib = Str.theFunctionsLibrary @ makeList compiledFuns
      where
        compiledFuns :: [Term Natural]
        compiledFuns =
          (OpQuote # (666 :: Natural)) -- TODO we have this unused term so that indices match. Remove it and adjust as needed
            : ( makeLibraryFunction
                  <$> [(f ^. compilerFunctionName, runCompilerFunction compilerCtx f) | f <- libFuns]
              )

    makeLibraryFunction :: (Text, Term Natural) -> Term Natural
    makeLibraryFunction (funName, c) =
      ("def-" <> funName)
        @ makeClosure
          ( \p ->
              let nockNilHere = nockNilTagged ("makeLibraryFunction-" <> show p)
               in case p of
                    FunCode -> ("funCode-" <> funName) @ c
                    ArgsTuple -> ("argsTuple-" <> funName) @ argsTuplePlaceholder "libraryFunction"
                    FunctionsLibrary -> ("functionsLibrary-" <> funName) @ functionsLibraryPlaceHolder
                    StandardLibrary -> ("stdlib-" <> funName) @ stdlibPlaceHolder
                    ClosureTotalArgsNum -> ("closureTotalArgsNum-" <> funName) @ nockNilHere
                    ClosureArgsNum -> ("closureArgsNum-" <> funName) @ nockNilHere
                    ClosureArgs -> ("closureArgs-" <> funName) @ nockNilHere
                    AnomaGetOrder -> ("anomaGetOrder-" <> funName) @ nockNilHere
          )

    makeMainFunction :: Term Natural -> Term Natural
    makeMainFunction c = makeClosure $ \p ->
      let nockNilHere = nockNilTagged ("makeMainFunction-" <> show p)
       in case p of
            FunCode -> run . runReader compilerCtx $ mainFunctionWrapper funcsLib c
            ArgsTuple -> argsTuplePlaceholder "mainFunction"
            FunctionsLibrary -> functionsLibraryPlaceHolder
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
            { _functionInfoPath = pathFromEnum FunctionsLibrary ++ indexStack i,
              _functionInfoArity = _compilerFunctionArity,
              _functionInfoName = _compilerFunctionName
            }
        )

stdlibPlaceHolder :: Term Natural
stdlibPlaceHolder =
  TermAtom
    Atom
      { _atomInfo =
          AtomInfo
            { _atomInfoLoc = Irrelevant Nothing,
              _atomInfoTag = Nothing,
              _atomInfoHint = Just AtomHintStdlibPlaceholder
            },
        _atom = 0 :: Natural
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

-- | Call a function with the passed arguments
callFunWithArgs ::
  forall r.
  (Members '[Reader CompilerCtx] r) =>
  FunctionId ->
  [Term Natural] ->
  Sem r (Term Natural)
callFunWithArgs fun args = do
  newSubject <- replaceArgs args
  fpath <- getFunctionPath fun
  fname <- getFunctionName fun
  let p' = fpath ++ closurePath FunCode
  return (opCall ("callFun-" <> fname) p' newSubject)

callClosure :: (Members '[Reader CompilerCtx] r) => TempRef -> [Term Natural] -> Sem r (Term Natural)
callClosure ref newArgs = do
  closure <- addressTempRef ref
  let oldArgsNum = getClosureField ClosureArgsNum closure
      oldArgs = getClosureField ClosureArgs closure
  allArgs <- appendToTuple oldArgs oldArgsNum (foldTermsOrNil newArgs) (nockIntegralLiteral (length newArgs))
  newSubject <- replaceSubject $ \case
    FunCode -> Just (getClosureField FunCode closure)
    ArgsTuple -> Just allArgs
    FunctionsLibrary -> Nothing
    StandardLibrary -> Nothing
    ClosureArgs -> Nothing
    ClosureTotalArgsNum -> Nothing
    ClosureArgsNum -> Nothing
    AnomaGetOrder -> Nothing
  return (opCall "callClosure" (closurePath FunCode) newSubject)

replaceSubject :: (Member (Reader CompilerCtx) r) => (AnomaCallablePathId -> Maybe (Term Natural)) -> Sem r (Term Natural)
replaceSubject = replaceSubject' "replaceSubject"

replaceSubject' :: (Member (Reader CompilerCtx) r) => Text -> (AnomaCallablePathId -> Maybe (Term Natural)) -> Sem r (Term Natural)
replaceSubject' tag f = do
  lst <- forM allElements $ \s -> do
    case f s of
      Nothing -> opAddress tag <$> stackPath s
      Just t' -> return t'
  return $ remakeList lst

replaceArgsWithTerm :: (Member (Reader CompilerCtx) r) => Text -> Term Natural -> Sem r (Term Natural)
replaceArgsWithTerm tag term =
  replaceSubject' ("replaceArgsWithTerm-" <> tag) $ \case
    ArgsTuple -> Just term
    _ -> Nothing

-- | Replace the arguments in the ArgsTuple stack with the passed arguments.
-- Resets the temporary stack to empty. Returns the new subject.
replaceArgs :: (Member (Reader CompilerCtx) r) => [Term Natural] -> Sem r (Term Natural)
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

-- Creates a case command from the reference `ref` to the compiled value and the
-- compiled branches.
caseCmd ::
  forall r.
  (Members '[Reader CompilerCtx] r) =>
  TempRef ->
  Maybe (Term Natural) ->
  [(Tree.Tag, Term Natural)] ->
  Sem r (Term Natural)
caseCmd ref defaultBranch = \case
  [] -> return (fromJust defaultBranch)
  (tag, b) : bs -> case tag of
    Tree.BuiltinTag t -> case nockmaBuiltinTag t of
      NockmaBuiltinBool v -> goBoolTag v b bs
    Tree.UserTag {} -> do
      rep <- getConstructorMemRep tag
      case rep of
        NockmaMemRepConstr -> goRepConstr tag b bs
        NockmaMemRepTuple
          | null bs, isNothing defaultBranch -> return b
          | otherwise -> error "redundant branch. Impossible?"
        NockmaMemRepList constr -> do
          bs' <- mapM (firstM asNockmaMemRepListConstr) bs
          goRepList ((constr, b) :| bs')
        NockmaMemRepMaybe constr -> do
          bs' <- mapM (firstM asNockmaMemRepMaybeConstr) bs
          goRepMaybe ((constr, b) :| bs')
  where
    goRepConstr ::
      Tree.Tag ->
      Term Natural ->
      [(Tree.Tag, Term Natural)] ->
      Sem r (Term Natural)
    goRepConstr tag b bs = do
      arg <- addressTempRef ref
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

    asNockmaMemRepMaybeConstr :: Tree.Tag -> Sem r NockmaMemRepMaybeConstr
    asNockmaMemRepMaybeConstr tag = case tag of
      Tree.UserTag {} -> do
        rep <- getConstructorMemRep tag
        case rep of
          NockmaMemRepMaybe constr -> return constr
          _ -> impossible
      Tree.BuiltinTag {} -> impossible

    goBoolTag ::
      Bool ->
      Term Natural ->
      [(Tree.Tag, Term Natural)] ->
      Sem r (Term Natural)
    goBoolTag v b bs = do
      arg <- addressTempRef ref
      let otherBranch = fromMaybe crash (firstJust f bs <|> defaultBranch)
      return $
        if
            | v -> branch arg b otherBranch
            | otherwise -> branch arg otherBranch b
      where
        f :: (Tree.Tag, Term Natural) -> Maybe (Term Natural)
        f (tag', br) = case tag' of
          Tree.UserTag {} -> impossible
          Tree.BuiltinTag tag -> case nockmaBuiltinTag tag of
            NockmaBuiltinBool v' -> guard (v /= v') $> br

    goRepList :: NonEmpty (NockmaMemRepListConstr, Term Natural) -> Sem r (Term Natural)
    goRepList ((c, b) :| bs) = do
      arg <- addressTempRef ref
      let cond = OpIsCell # arg
          otherBranch = fromMaybe crash (firstJust f bs <|> defaultBranch)
      return $ case c of
        NockmaMemRepListConstrCons -> branch cond b otherBranch
        NockmaMemRepListConstrNil -> branch cond otherBranch b
      where
        f :: (NockmaMemRepListConstr, Term Natural) -> Maybe (Term Natural)
        f (c', br) = guard (c /= c') $> br

    goRepMaybe :: NonEmpty (NockmaMemRepMaybeConstr, Term Natural) -> Sem r (Term Natural)
    goRepMaybe ((c, b) :| bs) = do
      arg <- addressTempRef ref
      let cond = OpIsCell # arg
          otherBranch = fromMaybe crash (firstJust f bs <|> defaultBranch)
      return $ case c of
        NockmaMemRepMaybeConstrJust -> branch cond b otherBranch
        NockmaMemRepMaybeConstrNothing -> branch cond otherBranch b
      where
        f :: (NockmaMemRepMaybeConstr, Term Natural) -> Maybe (Term Natural)
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

getConstructorField :: ConstructorPathId -> Term Natural -> Term Natural
getConstructorField = getField

getField :: (Enum field) => field -> Term Natural -> Term Natural
getField field t = t >># opAddress "getField" (pathFromEnum field)

getFieldInSubject :: (Member (Reader CompilerCtx) r) => (Enum field) => field -> Sem r (Term Natural)
getFieldInSubject field = do
  path <- stackPath field
  return $ opAddress "getFieldInSubject" path

getConstructorMemRep :: (Members '[Reader CompilerCtx] r) => Tree.Tag -> Sem r NockmaMemRep
getConstructorMemRep tag = (^. constructorInfoMemRep) <$> getConstructorInfo tag

crash :: Term Natural
crash = ("crash" @ OpAddress # OpAddress # OpAddress)

mul :: (Member (Reader CompilerCtx) r) => Term Natural -> Term Natural -> Sem r (Term Natural)
mul a b = callStdlib StdlibMul [a, b]

pow2 :: (Member (Reader CompilerCtx) r) => Term Natural -> Sem r (Term Natural)
pow2 x = callStdlib StdlibPow2 [x]

add :: (Member (Reader CompilerCtx) r) => Term Natural -> Term Natural -> Sem r (Term Natural)
add a b = callStdlib StdlibAdd [a, b]

sub :: (Member (Reader CompilerCtx) r) => Term Natural -> Term Natural -> Sem r (Term Natural)
sub a b = callStdlib StdlibSub [a, b]

dec :: (Member (Reader CompilerCtx) r) => Term Natural -> Sem r (Term Natural)
dec x = callStdlib StdlibDec [x]

intToUInt8 :: (Member (Reader CompilerCtx) r) => Term Natural -> Sem r (Term Natural)
intToUInt8 i = callStdlib StdlibMod [i, nockIntegralLiteral @Natural (2 ^ uint8Size)]
  where
    uint8Size :: Natural
    uint8Size = 8
