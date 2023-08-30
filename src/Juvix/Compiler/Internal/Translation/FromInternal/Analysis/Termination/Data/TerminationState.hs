module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.TerminationState
  ( IsTerminating (..),
    TerminationState,
    iniTerminationState,
    addTerminating,
    terminationTable,
    terminationFailedSet,
  )
where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data IsTerminating
  = -- | Has been checked or marked for termination.
    TerminatingCheckedOrMarked
  | -- | Has been checked for termination but failed.
    TerminatingFailed

data TerminationState = TerminationState
  { _iterminationTable :: HashMap FunctionName IsTerminating,
    _iterminationFailed :: HashSet FunctionName
  }

makeLenses ''TerminationState

iniTerminationState :: TerminationState
iniTerminationState =
  TerminationState
    { _iterminationTable = mempty,
      _iterminationFailed = mempty
    }

addTerminating :: (Members '[State TerminationState] r) => FunctionName -> IsTerminating -> Sem r ()
addTerminating f i = do
  modify' (set (iterminationTable . at f) (Just i))
  when isFailed (modify' (over iterminationFailed (HashSet.insert f)))
  where
    isFailed :: Bool
    isFailed = case i of
      TerminatingFailed -> True
      TerminatingCheckedOrMarked -> False

terminationTable :: SimpleGetter TerminationState (HashMap FunctionName IsTerminating)
terminationTable = iterminationTable

terminationFailedSet :: SimpleGetter TerminationState (HashSet FunctionName)
terminationFailedSet = iterminationFailed
