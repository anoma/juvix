module Parallel.ProgressLog where

import GHC.Conc (ThreadId)
import Juvix.Data.CodeAnn
import Juvix.Prelude.Base

data ProgressLog :: Effect where
  ProgressLog :: LogItem -> ProgressLog m ()

data ProgressLogOptions = ProgressLogOptions
  { _progressLogOptionsUseColors :: Bool,
    _progressLogOptionsShowThreadId :: Bool
  }

data LogItem = LogItem
  { _logItemThreadId :: ThreadId,
    _logItemMessage :: Doc CodeAnn
  }

makeSem ''ProgressLog
makeLenses ''ProgressLogOptions
makeLenses ''LogItem

runProgressLogIO :: (Members '[EmbedIO] r) => ProgressLogOptions -> Sem (ProgressLog ': r) a -> Sem r a
runProgressLogIO ProgressLogOptions {..} = interpret $ \case
  ProgressLog LogItem {..} -> do
    let threadDoc :: Maybe (Doc CodeAnn) = do
          guard _progressLogOptionsShowThreadId
          return (kwBracketL <> show _logItemThreadId <> kwBracketR)
    hRenderIO _progressLogOptionsUseColors stderr (threadDoc <?+> _logItemMessage <> hardline)

ignoreProgressLog :: Sem (ProgressLog ': r) a -> Sem r a
ignoreProgressLog = interpret $ \case
  ProgressLog {} -> return ()
