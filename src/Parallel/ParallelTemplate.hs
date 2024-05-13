{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Parallel.ParallelTemplate
  ( CompileArgs (..),
    compileArgsDependencies,
    compileArgsNodesIndex,
    compileArgsNodeName,
    compileArgsNumWorkers,
    compileArgsCompileNode,
    compile,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TVar (stateTVar)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Effectful.Concurrent
import Effectful.Concurrent.STM as STM
import Juvix.Prelude

data CompileArgs (s :: [Effect]) nodeId node compileProof = CompileArgs
  { _compileArgsNodesIndex :: NodesIndex nodeId node,
    _compileArgsDependencies :: Dependencies nodeId,
    _compileArgsNodeName :: node -> Text,
    _compileArgsNumWorkers :: Int,
    _compileArgsCompileNode :: HashMap nodeId compileProof -> node -> Sem s compileProof
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

newtype Logs = Logs
  { _logQueue :: TQueue Text
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

compilationStateFinished :: CompilationState nodeId compileProof -> Bool
compilationStateFinished CompilationState {..} = _compilationFinishedNum == _compilationTotalNum

addCompiledModule ::
  forall nodeId proof.
  (Hashable nodeId) =>
  Dependencies nodeId ->
  nodeId ->
  proof ->
  CompilationState nodeId proof ->
  (CompilationState nodeId proof, (Bool, [nodeId]))
addCompiledModule deps uid proof st = run . runState st $ do
  let revDeps :: [nodeId] = deps ^. dependenciesTableReverse . at uid . _Just . to toList
  modify @(CompilationState nodeId proof) (set (compilationState . at uid) (Just proof))
  modify @(CompilationState nodeId proof) (over compilationFinishedNum succ)
  isLast <- compilationStateFinished <$> get @(CompilationState nodeId proof)
  fmap (isLast,) . execOutputList . forM_ revDeps $ \s -> do
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
nodeDependencies deps m = fromMaybe mempty (deps ^. dependenciesTable . at m)

compile ::
  forall nodeId node compileProof r.
  (Hashable nodeId, Members '[IOE, Concurrent] r) =>
  CompileArgs r nodeId node compileProof ->
  Sem r ()
compile args@CompileArgs {..} = do
  let modsIx = _compileArgsNodesIndex
      deps = _compileArgsDependencies
      numMods :: Natural = fromIntegral (length (modsIx ^. nodesIndex))
      starterModules = [m | m <- HashMap.keys (modsIx ^. nodesIndex), null (nodeDependencies deps m)]

  logs <- Logs <$> newTQueueIO
  qq <- newTBQueueIO numMods
  let compileQ = CompileQueue qq
  forM_ starterModules (atomically . writeTBQueue qq)
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
    . runReader modsIx
    . runReader args
    . runReader logs
    . runReader compileQ
    . runReader deps
    . crashOnError
    $ do
      void (forkIO handleLogs)
      replicateM_ _compileArgsNumWorkers
        . forkIO
        $ lookForWork @nodeId @node @compileProof
      waitForWorkers @nodeId @compileProof

handleLogs :: (Members '[EmbedIO, Concurrent, Reader Logs] r) => Sem r ()
handleLogs = do
  x <- asks (^. logQueue) >>= atomically . readTQueue
  putStrLn x
  handleLogs

waitForWorkers ::
  forall nodeId compileProof r.
  ( Members
      '[ Concurrent,
         Reader (TVar (CompilationState nodeId compileProof)),
         Reader Logs
       ]
      r
  ) =>
  Sem r ()
waitForWorkers = do
  Logs logs <- ask
  cstVar <- ask @(TVar (CompilationState nodeId compileProof))
  allDone <-
    atomically $
      andM
        [ compilationStateFinished <$> readTVar cstVar,
          isEmptyTQueue logs
        ]
  unless allDone (waitForWorkers @nodeId @compileProof)

lookForWork ::
  forall nodeId node compileProof (s :: [Effect]) r.
  ( Hashable nodeId,
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
  tid <- myThreadId
  (compSt, nextModule) <- atomically $ do
    nextModule <- readTBQueue qq
    compSt <- readTVar stVar
    modifyTVar stVar (over compilationStartedNum succ)
    let num = compSt ^. compilationStartedNum
        total = compSt ^. compilationTotalNum
        progress = "[" <> show (succ num) <> " of " <> show total <> "] "
    logMsg (Just tid) logs (progress <> "Compiling " <> "TODO module name")
    return (compSt, nextModule)
  compileNode @s @nodeId @node @compileProof compSt nextModule
  lookForWork @nodeId @node @compileProof @s

getNode ::
  forall nodeId node r.
  (Hashable nodeId, Members '[Reader (NodesIndex nodeId node)] r) =>
  nodeId ->
  Sem r node
getNode uid = asks (^?! nodesIndex . at uid . _Just)

compileNode ::
  forall s nodeId node compileProof r.
  ( Hashable nodeId,
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
  CompilationState nodeId compileProof ->
  nodeId ->
  Sem r ()
compileNode st0 nodId = do
  m :: node <- getNode nodId
  deps <- ask @(Dependencies nodeId)
  let mdeps :: HashSet nodeId = nodeDependencies deps nodId
  -- checks that all dependencies have already been compiled
  let checkDep :: nodeId -> Sem r compileProof
      checkDep uid = case st0 ^. compilationState . at uid of
        Nothing -> throw Unexpected
        Just x -> return x
  depsProofs :: HashMap nodeId compileProof <- hashMapFromHashSetM checkDep mdeps
  compileFun <- asks @(CompileArgs s nodeId node compileProof) (^. compileArgsCompileNode)
  result <- inject (compileFun depsProofs m)
  registerCompiledModule @nodeId @node nodId result

registerCompiledModule ::
  forall nodeId node compileProof r.
  ( Hashable nodeId,
    Members
      '[ Concurrent,
         Reader (NodesIndex nodeId node),
         Reader (Dependencies nodeId),
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
  mutSt <- ask
  deps <- ask
  qq <- asks (^. compileQueue)
  tid <- myThreadId
  logs <- ask
  toQueue <- atomically $ do
    let msg :: Text = "Done compiling " <> "TODO <module name>"
    logMsg (Just tid) logs msg
    (isLast, toQueue) <- stateTVar mutSt (swap . addCompiledModule deps m proof)
    when isLast (logMsg Nothing logs "All work is done!")
    return toQueue
  forM_ toQueue (atomically . writeTBQueue qq)

logMsg :: Maybe ThreadId -> Logs -> Text -> STM ()
logMsg mtid (Logs q) msg = do
  let threadIdLabel = case mtid of
        Nothing -> ""
        Just tid -> "[" <> show tid <> "] "
  let msg' = threadIdLabel <> msg
  STM.writeTQueue q msg'
