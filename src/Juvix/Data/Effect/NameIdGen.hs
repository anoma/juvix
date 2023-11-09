module Juvix.Data.Effect.NameIdGen
  ( module Juvix.Data.Effect.NameIdGen,
    module Juvix.Data.NameId,
  )
where

import Data.Stream (Stream (Cons))
import Juvix.Data.NameId
import Juvix.Prelude.Base

data NameIdGenState = NameIdGenState
  { _nameIdGenStateModuleId :: ModuleId,
    _nameIdGenStateStream :: Stream Word64
  }

genNameIdState :: ModuleId -> NameIdGenState
genNameIdState mid = NameIdGenState mid ids
  where
    ids :: Stream Word64
    ids = aux minBound
    aux :: Word64 -> Stream Word64
    aux i = Cons i (aux (succ i))

data NameIdGen m a where
  FreshNameId :: NameIdGen m NameId

makeSem ''NameIdGen

toState :: Sem (NameIdGen ': r) a -> Sem (State NameIdGenState ': r) a
toState = reinterpret $ \case
  FreshNameId -> do
    NameIdGenState mid (Cons fresh rest) <- get
    put (NameIdGenState mid rest)
    return (NameId fresh mid)

runNameIdGen :: NameIdGenState -> Sem (NameIdGen ': r) a -> Sem r (NameIdGenState, a)
runNameIdGen s = runState s . toState

runTopNameIdGen :: ModuleId -> Sem (NameIdGen ': r) a -> Sem r (NameIdGenState, a)
runTopNameIdGen mid = runNameIdGen (genNameIdState mid)

evalTopNameIdGen :: ModuleId -> Sem (NameIdGen ': r) a -> Sem r a
evalTopNameIdGen mid = fmap snd . runTopNameIdGen mid
