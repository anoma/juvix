module Juvix.Data.NumThreads
  ( NumThreads (NumThreadsAuto),
    defaultNumThreads,
    numThreadsOne,
    numThreads,
    readNumThreads,
    mkNumThreads,
  )
where

import GHC.Conc as GHC
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude.Base.Foundation
import Prelude qualified

-- | The number of jobs must be at least 1
data NumThreads
  = NumThreads Int
  | NumThreadsAuto
  deriving stock (Eq, Ord, Generic)

instance Show NumThreads where
  show = \case
    NumThreads i -> show i
    NumThreadsAuto -> Str.auto

numThreadsOne :: NumThreads
numThreadsOne = NumThreads 1

-- | TODO improve auto with import tree
numThreads :: (MonadIO m) => NumThreads -> m Int
numThreads = \case
  NumThreads i -> return i
  NumThreadsAuto -> do
    nc <- liftIO GHC.getNumCapabilities
    return (max 1 (min 6 (nc - 2)))

defaultNumThreads :: NumThreads
defaultNumThreads = NumThreads 1

readNumThreads :: String -> Either String NumThreads
readNumThreads str
  | trim str == show NumThreadsAuto = return NumThreadsAuto
  | otherwise = readEither str >>= mkNumThreads

mkNumThreads :: Int -> Either String NumThreads
mkNumThreads n
  | n >= 1 = return (NumThreads n)
  | otherwise = Left "The number of threads has to be at least 1"
