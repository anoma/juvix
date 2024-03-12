module Juvix.Data.Effect.NameIdGenManager
  ( NameIdGenManager,
    genNameIdsFor,
    runNameIdGenManager,
    runTopNameIdGenManager,
    evalTopNameIdGenManager,
  )
where

import Juvix.Data.Effect.NameIdGen
import Juvix.Data.Effect.TaggedLock.Base
import Juvix.Prelude.Base
import Juvix.Prelude.Path

data NameIdGenManager m a where
  GetManagerState :: NameIdGenManager m NameIdGenManagerState
  ModifyManagerState :: (NameIdGenManagerState -> NameIdGenManagerState) -> NameIdGenManager m ()
  InternalWithModule :: ModuleId -> m a -> NameIdGenManager m a

newtype NameIdGenManagerState = NameIdGenManagerState
  { _nameIdGenManagerState :: HashMap ModuleId NameIdGenState
  }

makeLenses ''NameIdGenManagerState
makeSem ''NameIdGenManager

iniNameIdManagerState :: NameIdGenManagerState
iniNameIdManagerState = NameIdGenManagerState mempty

-- TODO revise
toRelPath :: ModuleId -> Path Rel File
toRelPath ModuleId {..} =
  relDir (unpack _moduleIdPackage)
    <//> relDir (unpack _moduleIdPackage)
    <//> relFile (unpack _moduleIdPackageVersion)

genNameIdsFor :: (Members '[NameIdGenManager] r) => ModuleId -> Sem (NameIdGen ': r) b -> Sem r b
genNameIdsFor moduleId x = do
  st <- fromMaybe iniNameIdState . (^. nameIdGenManagerState . at moduleId) <$> getManagerState
  (st' :: NameIdGenState, res) <- internalWithModule moduleId (runNameIdGen st moduleId x)
  modifyManagerState (set (nameIdGenManagerState . at moduleId) (Just st'))
  return res

re :: (Members '[TaggedLock] r) => Sem (NameIdGenManager ': r) a -> Sem (State NameIdGenManagerState ': r) a
re = reinterpretH $ \case
  GetManagerState {} -> get >>= pureT
  ModifyManagerState f -> modify f >>= pureT
  InternalWithModule md x -> do
    withTaggedLock (toRelPath md) (runTSimple x)

runNameIdGenManager ::
  (Members '[TaggedLock] r) =>
  NameIdGenManagerState ->
  Sem (NameIdGenManager ': r) a ->
  Sem r (NameIdGenManagerState, a)
runNameIdGenManager s = runState s . re

runTopNameIdGenManager :: (Members '[TaggedLock] r) => Sem (NameIdGenManager ': r) a -> Sem r (NameIdGenManagerState, a)
runTopNameIdGenManager = runNameIdGenManager iniNameIdManagerState

evalTopNameIdGenManager :: (Members '[TaggedLock] r) => Sem (NameIdGenManager ': r) a -> Sem r a
evalTopNameIdGenManager = fmap snd . runTopNameIdGenManager
