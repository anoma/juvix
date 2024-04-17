module Parallel.Experiment where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TVar (stateTVar)
import Effectful.Concurrent
import Effectful.Concurrent.STM as STM
import Juvix.Prelude
import Parallel.Base

data CompilationError = Unexpected
  deriving stock (Show)

newtype CompileQueue = CompileQueue
  { _compileQueue :: TBQueue ModuleSimpleId
  }

newtype Logs = Logs
  { _logQueue :: TQueue Text
  }

makeLenses ''Logs
makeLenses ''CompileQueue

main :: IO ()
main = compile bigModuleList

numWorkers :: Int
numWorkers = 3

crashOnError :: (Members '[EmbedIO] r) => Sem (Error CompilationError ': r) a -> Sem r a
crashOnError m = do
  x <- runError m
  case x of
    Right a -> return a
    Left e -> do
      print e >> exitFailure

compile :: [Module] -> IO ()
compile mods = runM . runConcurrent $ do
  let modsIx = mkModulesIndex mods
      deps = mkDependencies modsIx
      numMods :: Natural = fromIntegral (length mods)
      starterModules = [m ^. moduleId | m <- mods, null (moduleDependencies deps (m ^. moduleId))]

  logs <- Logs <$> newTQueueIO
  qq <- newTBQueueIO numMods
  let compileQ = CompileQueue qq
  forM_ starterModules (atomically . writeTBQueue qq)
  varCompilationState <-
    newTVarIO
      CompilationState
        { _compilationStartedNum = 0,
          _compilationFinishedNum = 0,
          _compilationTotalNum = numMods,
          _compilationPending = deps ^. dependenciesTable,
          _compilationState = mempty
        }
  runReader varCompilationState
    . runReader modsIx
    . runReader logs
    . runReader compileQ
    . runReader deps
    . crashOnError
    $ do
      void (forkIO handleLogs)
      replicateM_ numWorkers (forkIO lookForWork)
      waitForWorkers
      putStrLn "All work is done!"

handleLogs :: (Members '[EmbedIO, Concurrent, Reader Logs] r) => Sem r ()
handleLogs = do
  x <- asks (^. logQueue) >>= atomically . readTQueue
  putStrLn x
  handleLogs

waitForWorkers ::
  ( Members
      '[ Concurrent,
         Reader (TVar CompilationState),
         Reader Logs
       ]
      r
  ) =>
  Sem r ()
waitForWorkers = do
  Logs logs <- ask
  cstVar <- ask
  allDone <-
    atomically $
      andM
        [ compilationStateFinished <$> readTVar cstVar,
          isEmptyTQueue logs
        ]
  unless allDone waitForWorkers

allWorkIsFinished :: (Members '[Concurrent, Reader (TVar CompilationState)] r) => Sem r Bool
allWorkIsFinished = compilationStateFinished <$> (ask >>= atomically . readTVar)

lookForWork ::
  ( Members
      '[ Concurrent,
         Error CompilationError,
         Reader ModulesIndex,
         Reader Dependencies,
         Reader (TVar CompilationState),
         Reader CompileQueue,
         Reader Logs
       ]
      r
  ) =>
  Sem r ()
lookForWork = do
  qq <- asks (^. compileQueue)
  stVar <- ask
  logs <- ask
  tid <- myThreadId
  (compSt, nextModule) <- atomically $ do
    nextModule <- readTBQueue qq
    compSt <- readTVar stVar
    modifyTVar stVar (over compilationStartedNum succ)
    let num = compSt ^. compilationStartedNum
        total = compSt ^. compilationTotalNum
        progress = "[" <> show (succ num) <> " of " <> show total <> "] "
    logMsg tid logs (progress <> "Compiling " <> nextModule)
    return (compSt, nextModule)
  compileModule compSt nextModule
  lookForWork

compileModule ::
  forall r.
  ( Members
      '[ Concurrent,
         Error CompilationError,
         Reader ModulesIndex,
         Reader Dependencies,
         Reader (TVar CompilationState),
         Reader CompileQueue,
         Reader Logs
       ]
      r
  ) =>
  CompilationState ->
  ModuleSimpleId ->
  Sem r ()
compileModule st0 modId = do
  m <- getModule modId
  -- st0 <- readTVarIO mutSt
  -- checks that all dependencies have already been compiled
  let checkDep :: ModuleSimpleId -> Sem r ()
      checkDep uid = case st0 ^. compilationState . at uid of
        Nothing -> do
          -- void (error ("Dependency not found: " <> show uid))
          -- throw Unexpected
          return ()
        Just CompiledProof -> return ()
  forM_ (m ^. moduleDeps) checkDep
  -- the delay simulates compuation
  threadDelay (secondsToMicroseconds (m ^. moduleLoad))
  registerCompiledModule m

registerCompiledModule ::
  ( Members
      '[ Concurrent,
         Reader Dependencies,
         Reader (TVar CompilationState),
         Reader CompileQueue,
         Reader Logs
       ]
      r
  ) =>
  Module ->
  Sem r ()
registerCompiledModule m = do
  mutSt <- ask
  deps <- ask
  qq <- asks (^. compileQueue)
  tid <- myThreadId
  logs <- ask
  toQueue <- atomically $ do
    let msg = "Done compiling " <> m ^. moduleId
    logMsg tid logs msg
    writeTQueue (logs ^. logQueue) msg
    stateTVar mutSt (swap . addCompiledModule deps m)
  forM_ toQueue (atomically . writeTBQueue qq)

secondsToMicroseconds :: Double -> Int
secondsToMicroseconds x = floor (x * (10 ^ (6 :: Int)))

logMsg :: ThreadId -> Logs -> Text -> STM ()
logMsg tid (Logs q) msg = do
  let msg' = "[" <> show tid <> "] " <> msg
  STM.writeTQueue q msg'
