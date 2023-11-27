module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.TerminationState
  ( IsTerminating (..),
    TerminationState,
    iniTerminationState,
    addTerminating,
    safeToNormalize,
    terminationTable,
    terminationFailedSet,
  )
where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data IsTerminating
  = -- | Has been checked for termination
    TerminatingChecked
  | -- | Has failed termination checking but has been marked for termination
    TerminatingFailedMarked
  | -- | Has failed termination checking
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
  unless (passesTermination i) (modify' (over iterminationFailed (HashSet.insert f)))

safeToNormalize :: IsTerminating -> Bool
safeToNormalize = \case
  TerminatingFailed -> False
  TerminatingFailedMarked -> False
  TerminatingChecked -> True

passesTermination :: IsTerminating -> Bool
passesTermination = \case
  TerminatingFailed -> False
  TerminatingFailedMarked -> True
  TerminatingChecked -> True

terminationTable :: SimpleGetter TerminationState (HashMap FunctionName IsTerminating)
terminationTable = iterminationTable

terminationFailedSet :: SimpleGetter TerminationState (HashSet FunctionName)
terminationFailedSet = iterminationFailed
