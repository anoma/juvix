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
    compile,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TVar (stateTVar)
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

data LogQueueItem
  = LogQueueItem LogItem
  | -- | no more log items will be handled after this
    LogQueueClose

newtype Logs = Logs
  { _logQueue :: TQueue LogQueueItem
  }

newtype NodesIndex nodeId node = NodesIndex
  { _nodesIndex :: HashMap nodeId node
  }

data Task nodeId node = Task
  { _taskNum :: Natural,
    _taskTotal :: Natural,
    _taskNodeId :: nodeId,
    _taskNode :: node
  }

data Finished
  = -- | All modules have started compilation. They might still be compiling
    FinishedNoPending
  | FinishedPending

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

compilationStateFinished :: CompilationState nodeId compileProof -> Finished
compilationStateFinished CompilationState {..}
  | _compilationStartedNum == _compilationTotalNum = FinishedNoPending
  | _compilationStartedNum > _compilationTotalNum = impossible
  | otherwise = FinishedPending

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
      withAsync handleLogs $ \logHandler -> do
        replicateConcurrently_ _compileArgsNumWorkers $
          lookForWork @nodeId @node @compileProof
        wait logHandler
  (^. compilationState) <$> readTVarIO varCompilationState

handleLogs :: (Members '[ProgressLog, Concurrent, Reader Logs] r) => Sem r ()
handleLogs = do
  x <- asks (^. logQueue) >>= atomically . readTQueue
  case x of
    LogQueueClose -> return ()
    LogQueueItem l -> do
      progressLog l
      handleLogs

getTask ::
  forall nodeId (node :: GHCType) compileProof (s :: [Effect]) r.
  ( Hashable nodeId,
    Members
      '[ Concurrent,
         Reader (TVar (CompilationState nodeId compileProof)),
         Reader (CompileArgs s nodeId node compileProof),
         Reader (NodesIndex nodeId node),
         Reader (CompileQueue nodeId),
         Reader Logs
       ]
      r
  ) =>
  Sem r (Maybe (Task nodeId node))
getTask = do
  stVar <- ask @(TVar (CompilationState nodeId compileProof))
  qq <- asks (^. compileQueue)
  cstVar <- ask @(TVar (CompilationState nodeId compileProof))
  idx <- ask @(NodesIndex nodeId node)
  logs <- ask
  args <- ask @(CompileArgs s nodeId node compileProof)
  tid <- myThreadId
  atomically $ do
    finished <- compilationStateFinished <$> readTVar cstVar
    case finished of
      FinishedNoPending -> return Nothing
      FinishedPending -> do
        nextModuleId :: nodeId <- readTBQueue qq
        let n :: node =
              run
                . runReader idx
                $ getNode nextModuleId
        compSt <- readTVar stVar
        modifyTVar stVar (over compilationStartedNum succ)
        let num = succ (compSt ^. compilationStartedNum)
            total = compSt ^. compilationTotalNum
            name = annotate (AnnKind KNameTopModule) (pretty ((args ^. compileArgsNodeName) n))
            progress :: Doc CodeAnn =
              kwBracketL
                <> annotate AnnLiteralInteger (pretty num)
                <+> kwOf
                <+> annotate AnnLiteralInteger (pretty total) <> kwBracketR <> " "
            kwCompiling = annotate AnnKeyword "Compiling"
            isLast = num == total
        logMsg tid logs (progress <> kwCompiling <> " " <> name)
        when isLast (logClose logs)
        return $
          Just
            Task
              { _taskNum = num,
                _taskTotal = total,
                _taskNodeId = nextModuleId,
                _taskNode = n
              }

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
  whenJustM (getTask @nodeId @node @compileProof @s) $ \Task {..} -> do
    compileNode @s @nodeId @node @compileProof _taskNodeId
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
  proof :: compileProof <- inject (compileFun m)
  registerCompiledModule @nodeId @node @s @compileProof nodId proof

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

logClose :: Logs -> STM ()
logClose (Logs q) = do
  STM.writeTQueue q LogQueueClose

logMsg :: ThreadId -> Logs -> Doc CodeAnn -> STM ()
logMsg tid (Logs q) msg = do
  STM.writeTQueue
    q
    ( LogQueueItem
        LogItem
          { _logItemMessage = msg,
            _logItemThreadId = tid
          }
    )
