module Juvix.Data.NumThreads
  ( NumThreads,
    defaultNumThreads,
    numThreadsOne,
    numThreads,
    mkNumThreads,
    mkNumThreads',
  )
where

import GHC.Conc
import Juvix.Prelude.Base.Foundation
import Prelude qualified

-- | The number of jobs must be at least 1
newtype NumThreads = NumThreads Int
  deriving stock (Eq, Ord, Generic)

instance Show NumThreads where
  show (NumThreads i) = show i

numThreadsOne :: NumThreads
numThreadsOne = NumThreads 1

numThreads :: NumThreads -> Int
numThreads (NumThreads i) = i

defaultNumThreads :: NumThreads
defaultNumThreads = NumThreads (max 1 (numCapabilities - 2))

mkNumThreads :: Maybe Int -> Either String NumThreads
mkNumThreads = \case
  Nothing -> Right defaultNumThreads
  Just n
    | n >= 1 -> return (NumThreads n)
    | otherwise -> Left "The number of threads has to be at least 1"

mkNumThreads' :: Maybe Int -> NumThreads
mkNumThreads' = either (error . pack) id . mkNumThreads
