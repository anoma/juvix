module Parallel.ProgressLog2 where

import Juvix.Data.CodeAnn
import Juvix.Data.Logger
import Juvix.Data.TopModulePathKey
import Juvix.Prelude.Base

data ProgressLog2 :: Effect where
  ProgressLog2 :: LogItem2 -> ProgressLog2 m ()

newtype ProgressLogOptions2 = ProgressLogOptions2
  { _progressLogOptions2ShowThreadId :: Bool
  }

data RecompileReason

data LogItem2 = LogItem2
  { _logItem2Module :: TopModulePathKey,
    _logItem2Message :: Doc CodeAnn
  }

makeSem ''ProgressLog2
makeLenses ''ProgressLogOptions2
makeLenses ''LogItem2

defaultProgressLogOptions2 :: ProgressLogOptions2
defaultProgressLogOptions2 =
  ProgressLogOptions2
    { _progressLogOptions2ShowThreadId = False
    }

runProgressLog2 :: (Members '[Logger] r) => ProgressLogOptions2 -> Sem (ProgressLog2 ': r) a -> Sem r a
runProgressLog2 ProgressLogOptions2 {..} = undefined

ignoreProgressLog2 :: Sem (ProgressLog2 ': r) a -> Sem r a
ignoreProgressLog2 = interpret $ \case
  ProgressLog2 {} -> return ()
