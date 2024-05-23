module Juvix.Data.NumJobs
  ( NumJobs,
    defaultNumJobs,
    numJobsOne,
    numJobs,
    mkNumJobs,
    mkNumJobs',
  )
where

import GHC.Conc
import Juvix.Prelude.Base.Foundation
import Prelude qualified

-- | The number of jobs must be at least 1
newtype NumJobs = NumJobs Int
  deriving stock (Eq, Ord, Generic)

instance Show NumJobs where
  show (NumJobs i) = show i

numJobsOne :: NumJobs
numJobsOne = NumJobs 1

numJobs :: NumJobs -> Int
numJobs (NumJobs i) = i

defaultNumJobs :: NumJobs
defaultNumJobs = NumJobs (max 1 (numCapabilities - 2))

mkNumJobs :: Maybe Int -> Either String NumJobs
mkNumJobs = \case
  Nothing -> Right defaultNumJobs
  Just n
    | n >= 1 -> return (NumJobs n)
    | otherwise -> Left "The number of jobs cannot be negative"

mkNumJobs' :: Maybe Int -> NumJobs
mkNumJobs' = either (error . pack) id . mkNumJobs
