module Juvix.Data.Effect.NameIdGen2 where

import Data.Stream (Stream (Cons))
import Juvix.Data.NameId
import Juvix.Prelude.Base

data NameIdGenCoordinator m a

makeSem ''NameIdGenCoordinator

newtype NameIdGenCoordinatorState = NameIdGenCoordinatorState
  { _nameIdGenCoordinatorState :: HashMap ModuleId NameIdGenState
  }

newtype NameIdGenState = NameIdGenState
  { _nameIdGenStateStream :: Stream Word64
  }

newtype NameIdGenCtx = NameIdGenCtx
  { _nameIdGenCtxModuleId :: ModuleId
  }

makeLenses ''NameIdGenState
makeLenses ''NameIdGenCoordinatorState
makeLenses ''NameIdGenCtx

genNameIdState :: NameIdGenState
genNameIdState = NameIdGenState (streamFrom minBound)
  where
    streamFrom :: Word64 -> Stream Word64
    streamFrom i = Cons i (streamFrom (succ i))

re :: Sem (NameIdGenCoordinator ': r) a -> Sem (State NameIdGenState ': r) a
re = reinterpret $ \case {}

runNameIdGenDefaultModule :: NameIdGenState -> Sem (NameIdGenCoordinator ': r) a -> Sem r (NameIdGenState, a)
runNameIdGenDefaultModule s = runState s . re

runNameIdGen :: NameIdGenState -> Sem (NameIdGenCoordinator ': r) a -> Sem r (NameIdGenState, a)
runNameIdGen s = runState s . re

runTopNameIdGen :: Sem (NameIdGenCoordinator ': r) a -> Sem r (NameIdGenState, a)
runTopNameIdGen = runNameIdGen genNameIdState

evalTopNameIdGen :: Sem (NameIdGenCoordinator ': r) a -> Sem r a
evalTopNameIdGen = fmap snd . runTopNameIdGen
