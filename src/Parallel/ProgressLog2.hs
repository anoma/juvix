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
    _logItem2ThreadId :: ThreadId,
    _logItem2Action :: CompileAction,
    _logItem2Message :: Doc CodeAnn
  }

data ProgressLogState = ProgressLogState
  { _stateProcessed :: Natural,
    _stateProcessedPerPackage :: HashMap (Path Abs Dir) Natural
  }

data LogQueueItem
  = LogQueueItem LogItemDetails
  | -- | no more log items will be handled after this
    LogQueueClose

data LogKind
  = LogMainPackage
  | LogDependency

data LogItemDetails = LogItemDetails
  { _logItemDetailsNumber :: Natural,
    _logItemDetailsKind :: LogKind,
    _logItemDetailsLog :: LogItem2
  }

makeSem ''ProgressLog2
makeLenses ''ProgressLogOptions2
makeLenses ''LogItem2
makeLenses ''ProgressLogState
makeLenses ''LogItemDetails

-- defaultProgressLogOptions2 :: Path Abs Dir -> ImportTree -> ProgressLogOptions2
-- defaultProgressLogOptions2 root tree =
--   ProgressLogOptions2
--     { _progressLogOptions2ShowThreadId = False,
--       _progressLogOptions2ImportTree = tree,
--       _progressLogOptions2PackageRoot = root
--     }

iniProgressLogState :: ProgressLogState
iniProgressLogState =
  ProgressLogState
    { _stateProcessed = 0,
      _stateProcessedPerPackage = mempty
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
    tree :: ImportTree
    tree = opts ^. progressLogOptions2ImportTree

    numModulesByPackage :: HashMap (Path Abs Dir) Natural
    numModulesByPackage = fromIntegral . length <$> importTreeNodesByPackage tree

    packageSize :: Path Abs Dir -> Natural
    packageSize pkg = fromJust (numModulesByPackage ^. at pkg)

    mainPackage :: Path Abs Dir
    mainPackage = opts ^. progressLogOptions2PackageRoot

    handleLogs :: forall r'. (Members '[Logger, Concurrent] r') => TQueue LogQueueItem -> Sem r' ()
    handleLogs q = queueLoopWhile q $ \case
      LogQueueClose -> do
        logDebug (mkAnsiText (annotate AnnKeyword "Finished compilation"))
        return False
      LogQueueItem d -> do
        let l :: LogItem2
            l = d ^. logItemDetailsLog

            node :: ImportNode
            node = l ^. logItem2Module

            dependencyTag = case d ^. logItemDetailsKind of
              LogMainPackage -> Nothing
              LogDependency -> Just (annotate AnnKeyword "Dependency" <+> pretty (node ^. importNodePackageRoot))

            num =
              annotate AnnLiteralInteger (pretty (d ^. logItemDetailsNumber))
                <+> kwOf
                <+> annotate AnnLiteralInteger (pretty (packageSize (node ^. importNodePackageRoot)))

            tid :: Maybe (Doc CodeAnn) = do
              guard (opts ^. progressLogOptions2ShowThreadId)
              return (annotate AnnImportant (pretty @String (show (l ^. logItem2ThreadId))))

            msg :: AnsiText
            msg =
              mkAnsiText $
                (brackets <$> tid)
                  <?+> (brackets <$> dependencyTag)
                  <?+> brackets num
                    <+> annotate AnnKeyword (pretty (l ^. logItem2Action))
                    <+> l ^. logItem2Message

        let loglvl = compileActionLogLevel (l ^. logItem2Action)
        logMessage loglvl msg
        return True

    handler :: TVar ProgressLogState -> TQueue LogQueueItem -> EffectHandlerFO ProgressLog2 r
    handler st logs = \case
      ProgressLog2 i ->
        atomically $ do
          (n, isLast) <- getNextNumber
          let k
                | fromMainPackage = LogMainPackage
                | otherwise = LogDependency
              d =
                LogItemDetails
                  { _logItemDetailsKind = k,
                    _logItemDetailsNumber = n,
                    _logItemDetailsLog = i
                  }
          STM.writeTQueue logs (LogQueueItem d)
          when isLast (STM.writeTQueue logs LogQueueClose)
        where
          fromPackage :: Path Abs Dir
          fromPackage = i ^. logItem2Module . importNodePackageRoot

          fromMainPackage :: Bool
          fromMainPackage = fromPackage == mainPackage

          totalModules :: Natural
          totalModules = importTreeSize (opts ^. progressLogOptions2ImportTree)

          getNextNumber :: STM (Natural, Bool)
          getNextNumber = do
            stateTVar st $ \old ->
              let processed = old ^. stateProcessed + 1
                  pkgProcessed = over (at fromPackage) (Just . maybe 1 succ) (old ^. stateProcessedPerPackage)
                  st' =
                    old
                      { _stateProcessed = processed,
                        _stateProcessedPerPackage = pkgProcessed
                      }
                  ret = (fromJust (pkgProcessed ^. at fromPackage), processed == totalModules)
               in (ret, st')

queueLoopWhile :: (Members '[Concurrent] r) => TQueue a -> (a -> Sem r Bool) -> Sem r ()
queueLoopWhile q f = do
  whileCond <- atomically (readTQueue q) >>= f
  when (whileCond) (queueLoopWhile q f)

ignoreProgressLog2 :: Sem (ProgressLog2 ': r) a -> Sem r a
ignoreProgressLog2 = interpret $ \case
  ProgressLog2 {} -> return ()
