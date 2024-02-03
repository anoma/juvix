module Juvix.Data.Effect.NameIdGen
  ( runNameIdGen,
    runTopNameIdGen,
    evalTopNameIdGen,
    freshNameId,
    genNameIdState,
    NameIdGen,
    NameIdGenState,
    module Juvix.Data.NameId,
  )
where

import Data.Stream (Stream (Cons))
import Juvix.Data.NameId
import Juvix.Prelude.Base

newtype NameIdGenState = NameIdGenState
  { _nameIdGenStateStream :: Stream Word64
  }

newtype NameIdGenCtx = NameIdGenCtx
  { _nameIdGenCtxModuleId :: ModuleId
  }

makeLenses ''NameIdGenState
makeLenses ''NameIdGenCtx

genNameIdState :: NameIdGenState
genNameIdState = NameIdGenState ids
  where
    ids :: Stream Word64
    ids = aux minBound
    aux :: Word64 -> Stream Word64
    aux i = Cons i (aux (succ i))

data NameIdGen m a where
  FreshNameId :: NameIdGen m NameId

makeSem ''NameIdGen

re :: Sem (NameIdGen ': r) a -> Sem (State NameIdGenState ': Reader NameIdGenCtx ': r) a
re = reinterpret2 $ \case
  FreshNameId -> do
    mid <- asks (^. nameIdGenCtxModuleId)
    Cons fresh rest <- gets (^. nameIdGenStateStream)
    put (NameIdGenState rest)
    return (NameId fresh mid)

runNameIdGen :: NameIdGenState -> ModuleId -> Sem (NameIdGen ': r) a -> Sem r (NameIdGenState, a)
runNameIdGen s c = runReader (NameIdGenCtx c) . runState s . re

runTopNameIdGen :: ModuleId -> Sem (NameIdGen ': r) a -> Sem r (NameIdGenState, a)
runTopNameIdGen = runNameIdGen genNameIdState

evalTopNameIdGen :: ModuleId -> Sem (NameIdGen ': r) a -> Sem r a
evalTopNameIdGen mid = fmap snd . runTopNameIdGen mid
