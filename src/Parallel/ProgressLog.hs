module Parallel.ProgressLog where

import Juvix.Data.CodeAnn
import Juvix.Prelude.Base

data ProgressLog :: Effect where
  ProgressLog :: Doc CodeAnn -> ProgressLog m ()

makeSem ''ProgressLog

runProgressLogIO :: (Members '[EmbedIO] r) => Bool -> Sem (ProgressLog ': r) a -> Sem r a
runProgressLogIO useColors = interpret $ \case
  ProgressLog l -> do
    renderIO useColors l
    renderIO useColors ("\n" :: Text)

ignoreProgressLog :: Sem (ProgressLog ': r) a -> Sem r a
ignoreProgressLog = interpret $ \case
  ProgressLog {} -> return ()
