module Parallel.ProgressLog2 where

import Control.Concurrent.STM.TVar (stateTVar)
import Effectful.Concurrent
import Effectful.Concurrent.STM as STM
import Juvix.Compiler.Concrete.Print.Base
import Juvix.Compiler.Pipeline.Driver.Data
import Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree.Base
import Juvix.Data.CodeAnn
import Juvix.Data.Logger
import Juvix.Prelude

data ProgressLog2 :: Effect where
  ProgressLog2 :: LogItem2 -> ProgressLog2 m ()

data ProgressLogOptions2 = ProgressLogOptions2
  { _progressLogOptions2ShowThreadId :: Bool,
    _progressLogOptions2PackageRoot :: Path Abs Dir,
    _progressLogOptions2ImportTree :: ImportTree
  }

data LogItem2 = LogItem2
  { _logItem2Module :: ImportNode,
    _logItem2Action :: CompileAction,
    _logItem2Message :: Doc CodeAnn
  }

data ProgressLogState = ProgressLogState
  { _stateProcessed :: Natural,
    _statePackageProcessed :: Natural
  }

data LogQueueItem
  = LogQueueItem LogItemDetails
  | -- | no more log items will be handled after this
    LogQueueClose

data LogKind
  = LogMainPackage Natural
  | LogDependency

data LogItemDetails = LogItemDetails
  { _logItemDetailsKind :: LogKind,
    _logItemDetailsLog :: LogItem2
  }

makeSem ''ProgressLog2
makeLenses ''ProgressLogOptions2
makeLenses ''LogItem2
makeLenses ''ProgressLogState
makeLenses ''LogItemDetails

defaultProgressLogOptions2 :: Path Abs Dir -> ImportTree -> ProgressLogOptions2
defaultProgressLogOptions2 root tree =
  ProgressLogOptions2
    { _progressLogOptions2ShowThreadId = False,
      _progressLogOptions2ImportTree = tree,
      _progressLogOptions2PackageRoot = root
    }

iniProgressLogState :: ProgressLogState
iniProgressLogState =
  ProgressLogState
    { _stateProcessed = 0,
      _statePackageProcessed = 0
    }

runProgressLog2 ::
  forall r a.
  (Members '[Logger, Concurrent] r) =>
  ProgressLogOptions2 ->
  Sem (ProgressLog2 ': r) a ->
  Sem r a
runProgressLog2 opts m = do
  logs <- newTQueueIO
  st :: TVar ProgressLogState <- newTVarIO iniProgressLogState
  withAsync (handleLogs logs) $ \logHandler -> do
    x <- interpret (handler st logs) m
    wait logHandler
    return x
  where
    handleLogs :: forall r'. (Members '[Logger, Concurrent] r') => TQueue LogQueueItem -> Sem r' ()
    handleLogs q = queueLoopWhile q $ \case
      LogQueueClose -> do
        logDebug (mkAnsiText (annotate AnnKeyword "Finished compilation"))
        return False
      LogQueueItem d -> do
        let l :: LogItem2
            l = d ^. logItemDetailsLog

            iden = case d ^. logItemDetailsKind of
              LogDependency -> annotate AnnKeyword "Dependency"
              LogMainPackage num ->
                annotate AnnLiteralInteger (pretty num)
                  <+> kwOf
                  <+> annotate AnnLiteralInteger (pretty packageNumModules)

            msg :: AnsiText
            msg =
              mkAnsiText $
                kwBracketL
                  <> iden
                  <> kwBracketR
                  <+> annotate AnnKeyword (pretty (l ^. logItem2Action))
                  <+> l ^. logItem2Message
        let loglvl = compileActionLogLevel (l ^. logItem2Action)
        logMessage loglvl msg
        return True
        where
          packageNumModules :: Natural
          packageNumModules = fromIntegral (length (importTreeProjectNodes (opts ^. progressLogOptions2PackageRoot) (opts ^. progressLogOptions2ImportTree)))

    handler :: TVar ProgressLogState -> TQueue LogQueueItem -> EffectHandlerFO ProgressLog2 r
    handler st logs = \case
      ProgressLog2 i ->
        atomically $ do
          (n, isLast) <- getNextNumber
          let k
                | fromMainPackage = LogMainPackage n
                | otherwise = LogDependency
              d =
                LogItemDetails
                  { _logItemDetailsKind = k,
                    _logItemDetailsLog = i
                  }
          STM.writeTQueue logs (LogQueueItem d)
          when isLast (STM.writeTQueue logs LogQueueClose)
        where
          fromMainPackage :: Bool
          fromMainPackage = i ^. logItem2Module . importNodePackageRoot == opts ^. progressLogOptions2PackageRoot

          totalModules :: Natural
          totalModules = importTreeSize (opts ^. progressLogOptions2ImportTree)

          getNextNumber :: STM (Natural, Bool)
          getNextNumber = do
            stateTVar st $ \old ->
              let processed = old ^. stateProcessed + 1
                  pkgProcessed = (if fromMainPackage then succ else id) (old ^. statePackageProcessed)
                  st' =
                    old
                      { _stateProcessed = processed,
                        _statePackageProcessed = pkgProcessed
                      }
                  ret = (pkgProcessed, processed == totalModules)
               in (ret, st')

queueLoopWhile :: (Members '[Concurrent] r) => TQueue a -> (a -> Sem r Bool) -> Sem r ()
queueLoopWhile q f = do
  whileCond <- atomically (readTQueue q) >>= f
  when (whileCond) (queueLoopWhile q f)

ignoreProgressLog2 :: Sem (ProgressLog2 ': r) a -> Sem r a
ignoreProgressLog2 = interpret $ \case
  ProgressLog2 {} -> return ()
