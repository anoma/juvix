module Parallel.ProgressLog where

import Juvix.Data.CodeAnn
import Juvix.Prelude.Base

data ProgressLog :: Effect where
  ProgressLog :: Doc CodeAnn -> ProgressLog m ()

makeSem ''ProgressLog

runProgressLogIO :: (Members '[EmbedIO] r) => Sem (ProgressLog ': r) a -> Sem r a
runProgressLogIO = interpret $ \case
  ProgressLog l -> do
    renderIO True l
    renderIO False ("\n" :: Text)

ignoreProgressLog :: Sem (ProgressLog ': r) a -> Sem r a
ignoreProgressLog = interpret $ \case
  ProgressLog {} -> return ()
