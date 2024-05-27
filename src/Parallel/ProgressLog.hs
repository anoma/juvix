module Parallel.ProgressLog where

import Juvix.Data.CodeAnn
import Juvix.Prelude.Base

data ProgressLog :: Effect where
  ProgressLog :: Doc CodeAnn -> ProgressLog m ()

makeSem ''ProgressLog

-- runProgressLogIO :: (Members '[EmbedIO] r) => Bool -> Sem (ProgressLog ': r) a -> Sem r a
runProgressLogIO :: Bool -> Sem (ProgressLog ': r) a -> Sem r a
runProgressLogIO _useColors = interpret $ \case
  ProgressLog _l -> do
    return ()

-- TODO restore when I know what's the best way to fix smoke tests
-- hRenderIO useColors stderr l
-- hRenderIO useColors stderr ("\n" :: Text)

ignoreProgressLog :: Sem (ProgressLog ': r) a -> Sem r a
ignoreProgressLog = interpret $ \case
  ProgressLog {} -> return ()
