{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Parallel.ParallelTemplate
  ( module Parallel.ProgressLog,
    CompileArgs (..),
    NodesIndex (..),
    Dependencies (..),
    compileArgsDependencies,
    compileArgsNodesIndex,
    compileArgsNodeName,
    compileArgsNumWorkers,
    compileArgsCompileNode,
    compileArgsPreProcess,
    compilationError,
    compile,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TVar (stateTVar)
import Control.Exception qualified as GHC
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Effectful.Concurrent
import Effectful.Concurrent.STM as STM
import Juvix.Data.CodeAnn
import Juvix.Prelude
import Parallel.ProgressLog

data CompileArgs (s :: [Effect]) nodeId node compileProof = CompileArgs
  { _compileArgsNodesIndex :: NodesIndex nodeId node,
    _compileArgsDependencies :: Dependencies nodeId,
    _compileArgsNodeName :: node -> Text,
    _compileArgsNumWorkers :: Int,
    -- | Called on every node without any specific order
    _compileArgsPreProcess :: Maybe (nodeId -> Sem s ()),
    _compileArgsCompileNode :: node -> Sem s compileProof
  }

data CompilationState nodeId compiledProof = CompilationState
  { _compilationState :: HashMap nodeId compiledProof,
    -- | Initially populated with `Dependencies._dependenciesTable`. It
    -- is used to keep track of which dependencies need to be compiled before
    -- this module is enqueued for compilation.
    _compilationPending :: HashMap nodeId (HashSet nodeId),
    _compilationStartedNum :: Natural,
    _compilationFinishedNum :: Natural,
    _compilationError :: Maybe JuvixError,
    _compilationTotalNum :: Natural
  }

data Dependencies nodeId = Dependencies
  { _dependenciesTable :: HashMap nodeId (HashSet nodeId),
    -- if x ∈ T[y], then y imports x
    _dependenciesTableReverse :: HashMap nodeId (HashSet nodeId)
  }
  deriving stock (Show)

data ParallelError = Unexpected
  deriving stock (Show)

newtype CompileQueue nodeId = CompileQueue
  { _compileQueue :: TBQueue nodeId
  }

newtype Logs = Logs
  { _logQueue :: TQueue LogItem
  }

newtype NodesIndex nodeId node = NodesIndex
  { _nodesIndex :: HashMap nodeId node
  }

makeLenses ''Logs
makeLenses ''NodesIndex
makeLenses ''CompileQueue
makeLenses ''Dependencies
makeLenses ''CompilationState
makeLenses ''CompileArgs

instance (Show nodeId, Pretty nodeId) => Pretty (Dependencies nodeId) where
  pretty d =
    itemize
      [ pretty from <> ":\n" <> indent' (itemize (pretty <$> toList deps))
        | (from, deps) <- HashMap.toList (d ^. dependenciesTable)
      ]

data Finished
  = FinishedOk
  | FinishedError JuvixError
  | FinishedNot

compilationStateFinished :: CompilationState nodeId compileProof -> Finished
compilationStateFinished CompilationState {..}
  | Just err <- _compilationError = FinishedError err
  | _compilationFinishedNum == _compilationTotalNum = FinishedOk
  | otherwise = FinishedNot

addCompiledModule ::
  forall nodeId proof.
  (Hashable nodeId) =>
  Dependencies nodeId ->
  nodeId ->
  proof ->
  CompilationState nodeId proof ->
  (CompilationState nodeId proof, [nodeId])
addCompiledModule deps uid proof st = run . runState st $ do
  let revDeps :: [nodeId] = deps ^. dependenciesTableReverse . at uid . _Just . to toList
  modify @(CompilationState nodeId proof) (set (compilationState . at uid) (Just proof))
  modify @(CompilationState nodeId proof) (over compilationFinishedNum succ)
  execOutputList . forM_ revDeps $ \s -> do
    modify @(CompilationState nodeId proof) (over (compilationPending . at s . _Just) (HashSet.delete uid))
    -- if there are no more pending dependencies, we push it to the queue
    pend <- gets @(CompilationState nodeId proof) (^. compilationPending . at s)
    case pend of
      Just p
        | null p -> output s
      _ -> return ()

crashOnError :: (Members '[EmbedIO] r) => Sem (Error ParallelError ': r) a -> Sem r a
crashOnError m = do
  x <- runError m
  case x of
    Right a -> return a
    Left e -> print e >> exitFailure

nodeDependencies :: (Hashable nodeId) => Dependencies nodeId -> nodeId -> HashSet nodeId
nodeDependencies deps m = fromMaybe impossible (deps ^. dependenciesTable . at m)

compile ::
  forall nodeId node compileProof r.
  ( Hashable nodeId,
    Members '[IOE, ProgressLog, Concurrent, Error JuvixError] r
  ) =>
  CompileArgs r nodeId node compileProof ->
  Sem r (HashMap nodeId compileProof)
compile args@CompileArgs {..} = do
  let nodesIx = _compileArgsNodesIndex
      allNodesIds :: [nodeId] = HashMap.keys (nodesIx ^. nodesIndex)
      deps = _compileArgsDependencies
      numMods :: Natural = fromIntegral (length allNodesIds)
      starterModules :: [nodeId] =
        [m | m <- allNodesIds, null (nodeDependencies deps m)]
  logs <- Logs <$> newTQueueIO
  qq <- newTBQueueIO (max 1 numMods)
  let compileQ = CompileQueue qq
  whenJust _compileArgsPreProcess $ \preProcess ->
    mapConcurrently_ preProcess allNodesIds
  atomically (forM_ starterModules (writeTBQueue qq))
  let iniCompilationState :: CompilationState nodeId compileProof =
        CompilationState
          { _compilationStartedNum = 0,
            _compilationFinishedNum = 0,
            _compilationTotalNum = numMods,
            _compilationError = Nothing,
            _compilationPending = deps ^. dependenciesTable,
            _compilationState = mempty
          }
  varCompilationState <- newTVarIO iniCompilationState
  runReader varCompilationState
    . runReader nodesIx
    . runReader args
    . runReader logs
    . runReader compileQ
    . runReader deps
    . crashOnError
    $ do
      let newThread ::
            forall r' a.
            (Members '[Concurrent] r') =>
            Sem r' a ->
            Sem r' ()
          newThread m = void . forkFinally m $ \case
            Left err -> GHC.throw err
            Right {} -> return ()
      withAsync handleLogs $ \_logHandler -> do
        let useAsync = False
        if
            | useAsync ->
                replicateConcurrently_ _compileArgsNumWorkers $
                  lookForWork @nodeId @node @compileProof
            | otherwise ->
                replicateM_ _compileArgsNumWorkers
                  . newThread
                  $ lookForWork @nodeId @node @compileProof
        waitForWorkers @nodeId @compileProof
  (^. compilationState) <$> readTVarIO varCompilationState

handleLogs :: (Members '[ProgressLog, Concurrent, Reader Logs] r) => Sem r ()
handleLogs = do
  x <- asks (^. logQueue) >>= atomically . readTQueue
  progressLog x
  handleLogs

waitForWorkers ::
  forall nodeId compileProof r.
  ( Members
      '[ Concurrent,
         Reader (TVar (CompilationState nodeId compileProof)),
         Error JuvixError,
         Reader Logs
       ]
      r
  ) =>
  Sem r ()
waitForWorkers = do
  Logs logs <- ask
  cstVar <- ask @(TVar (CompilationState nodeId compileProof))
  (finished, noMoreLogs) <- atomically $ do
    finished <- compilationStateFinished <$> readTVar cstVar
    noMoreLogs <- isEmptyTQueue logs
    return (finished, noMoreLogs)
  let waitMore = waitForWorkers @nodeId @compileProof
  case finished of
    FinishedError err
      | noMoreLogs -> throw err
      | otherwise -> waitMore
    FinishedNot -> waitMore
    FinishedOk -> unless noMoreLogs waitMore

lookForWork ::
  forall nodeId node compileProof (s :: [Effect]) r.
  ( Hashable nodeId,
    Members '[Error JuvixError] s,
    Members
      '[ Concurrent,
         Error ParallelError,
         Reader (NodesIndex nodeId node),
         Reader (CompileArgs s nodeId node compileProof),
         Reader (Dependencies nodeId),
         Reader (TVar (CompilationState nodeId compileProof)),
         Reader (CompileQueue nodeId),
         Reader Logs
       ]
      r,
    Subset s r
  ) =>
  Sem r ()
lookForWork = do
  qq <- asks (^. compileQueue)
  stVar <- ask @(TVar (CompilationState nodeId compileProof))
  logs <- ask
  args <- ask @(CompileArgs s nodeId node compileProof)
  idx <- ask @(NodesIndex nodeId node)
  tid <- myThreadId
  nextModule <- atomically $ do
    nextModule :: nodeId <- readTBQueue qq
    let n :: node = run . runReader idx $ getNode nextModule
        name = annotate (AnnKind KNameTopModule) (pretty ((args ^. compileArgsNodeName) n))
    compSt <- readTVar stVar
    modifyTVar stVar (over compilationStartedNum succ)
    let num = compSt ^. compilationStartedNum
        total = compSt ^. compilationTotalNum
        progress :: Doc CodeAnn =
          kwBracketL
            <> annotate AnnLiteralInteger (pretty (succ num))
            <+> kwOf
            <+> annotate AnnLiteralInteger (pretty total) <> kwBracketR <> " "
        kwCompiling = annotate AnnKeyword "Compiling"
    logMsg tid logs (progress <> kwCompiling <> " " <> name)
    return nextModule
  compileNode @s @nodeId @node @compileProof nextModule
  lookForWork @nodeId @node @compileProof @s @r

getNode ::
  forall nodeId node r.
  (Hashable nodeId, Members '[Reader (NodesIndex nodeId node)] r) =>
  nodeId ->
  Sem r node
getNode uid = asks (^?! nodesIndex . at uid . _Just)

compileNode ::
  forall s nodeId node compileProof r.
  ( Hashable nodeId,
    Members '[Error JuvixError] s,
    Members
      '[ Concurrent,
         Error ParallelError,
         Reader (CompileArgs s nodeId node compileProof),
         Reader (NodesIndex nodeId node),
         Reader (Dependencies nodeId),
         Reader (TVar (CompilationState nodeId compileProof)),
         Reader (CompileQueue nodeId),
         Reader Logs
       ]
      r,
    Subset s r
  ) =>
  nodeId ->
  Sem r ()
compileNode nodId = do
  m :: node <- getNode nodId
  compileFun <- asks @(CompileArgs s nodeId node compileProof) (^. compileArgsCompileNode)
  st :: TVar (CompilationState nodeId compileProof) <- ask
  result :: Either (CallStack, JuvixError) compileProof <-
    inject $
      tryError @JuvixError (compileFun m)
  case result of
    Left (_, err) -> atomically (modifyTVar st (set compilationError (Just err)))
    Right proof -> registerCompiledModule @nodeId @node @s @compileProof nodId proof

registerCompiledModule ::
  forall nodeId node s compileProof r.
  ( Hashable nodeId,
    Members
      '[ Concurrent,
         Reader (NodesIndex nodeId node),
         Reader (Dependencies nodeId),
         Reader (CompileArgs s nodeId node compileProof),
         Reader (TVar (CompilationState nodeId compileProof)),
         Reader (CompileQueue nodeId),
         Reader Logs
       ]
      r
  ) =>
  nodeId ->
  compileProof ->
  Sem r ()
registerCompiledModule m proof = do
  mutSt <- ask @((TVar (CompilationState nodeId compileProof)))
  deps <- ask
  qq <- asks (^. compileQueue)
  atomically $ do
    toQueue <- stateTVar mutSt (swap . addCompiledModule deps m proof)
    forM_ toQueue (writeTBQueue qq)

logMsg :: ThreadId -> Logs -> Doc CodeAnn -> STM ()
logMsg tid (Logs q) msg = do
  STM.writeTQueue
    q
    LogItem
      { _logItemMessage = msg,
        _logItemThreadId = tid
      }
