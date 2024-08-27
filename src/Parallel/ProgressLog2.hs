module Parallel.ProgressLog2 where

import Control.Concurrent.STM.TVar (stateTVar)
import Effectful.Concurrent
import Effectful.Concurrent.STM as STM
import Juvix.Compiler.Concrete.Print.Base
import Juvix.Data.CodeAnn
import Juvix.Data.Logger
import Juvix.Data.TopModulePathKey
import Juvix.Prelude

data ProgressLog2 :: Effect where
  ProgressLog2 :: LogItem2 -> ProgressLog2 m ()

data ProgressLogOptions2 = ProgressLogOptions2
  { _progressLogOptions2ShowThreadId :: Bool,
    _progressLogOptions2TotalModules :: Natural
  }

data LogItem2 = LogItem2
  { _logItem2Module :: TopModulePathKey,
    _logItem2LogLevel :: Maybe LogLevel,
    _logItem2Message :: Doc CodeAnn
  }

newtype ProgressLogState = ProgressLogState
  { _stateProcessed :: Natural
  }

data LogQueueItem
  = LogQueueItem LogItemDetails
  | -- | no more log items will be handled after this
    LogQueueClose

data LogItemDetails = LogItemDetails
  { _logItemDetailsNumber :: Natural,
    _logItemDetailsLog :: LogItem2
  }

makeSem ''ProgressLog2
makeLenses ''ProgressLogOptions2
makeLenses ''LogItem2
makeLenses ''ProgressLogState
makeLenses ''LogItemDetails

defaultProgressLogOptions2 :: Natural -> ProgressLogOptions2
defaultProgressLogOptions2 numMods =
  ProgressLogOptions2
    { _progressLogOptions2ShowThreadId = False,
      _progressLogOptions2TotalModules = numMods
    }

iniProgressLogState :: ProgressLogState
iniProgressLogState =
  ProgressLogState
    { _stateProcessed = 0
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
  withAsync (handleLogs opts logs) $ \_logHandler -> do
    interpret (handler st logs) m
  where
    handler :: TVar ProgressLogState -> TQueue LogQueueItem -> EffectHandlerFO ProgressLog2 r
    handler st logs = \case
      ProgressLog2 i ->
        atomically $ do
          n <- getNextNumber
          let d =
                LogItemDetails
                  { _logItemDetailsNumber = n,
                    _logItemDetailsLog = i
                  }
          STM.writeTQueue logs (LogQueueItem d)
      where
        getNextNumber :: STM Natural
        getNextNumber = do
          stateTVar st $ \old ->
            let n = old ^. stateProcessed + 1
             in (n, set stateProcessed n old)

queueLoopWhile :: (Members '[Concurrent] r) => TQueue a -> (a -> Sem r Bool) -> Sem r ()
queueLoopWhile q f = do
  whileCond <- atomically (readTQueue q) >>= f
  when (whileCond) (queueLoopWhile q f)

handleLogs :: (Members '[Logger, Concurrent] r) => ProgressLogOptions2 -> TQueue LogQueueItem -> Sem r ()
handleLogs opts q = queueLoopWhile q $ \case
  LogQueueClose -> return False
  LogQueueItem d -> do
    let l :: LogItem2
        l = d ^. logItemDetailsLog
        msg :: AnsiText
        msg =
          mkAnsiText $
            kwBracketL
              <> annotate AnnLiteralInteger (pretty (d ^. logItemDetailsNumber))
              <+> kwOf
              <+> annotate AnnLiteralInteger (pretty (opts ^. progressLogOptions2TotalModules))
                <> kwBracketR
              <+> l ^. logItem2Message
    case l ^. logItem2LogLevel of
      Nothing -> logProgress msg
      Just loglvl -> logMessage loglvl msg
    return True

ignoreProgressLog2 :: Sem (ProgressLog2 ': r) a -> Sem r a
ignoreProgressLog2 = interpret $ \case
  ProgressLog2 {} -> return ()
