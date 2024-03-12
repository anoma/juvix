module Juvix.Data.Effect.NameIdGen
  ( runNameIdGen,
    runNameIdGenDefaultModule,
    runTopNameIdGen,
    evalTopNameIdGen,
    freshNameId,
    iniNameIdState,
    NameIdGen,
    NameIdGenState,
    module Juvix.Data.NameId,
  )
where

import Data.Stream (Stream (Cons))
import Juvix.Data.NameId
import Juvix.Prelude.Base

data NameIdGen m a where
  FreshNameId :: NameIdGen m NameId

makeSem ''NameIdGen

newtype NameIdGenState = NameIdGenState
  { _nameIdGenStateStream :: Stream Word64
  }

newtype NameIdGenCtx = NameIdGenCtx
  { _nameIdGenCtxModuleId :: ModuleId
  }

makeLenses ''NameIdGenState
makeLenses ''NameIdGenCtx

iniNameIdState :: NameIdGenState
iniNameIdState = NameIdGenState ids
  where
    ids :: Stream Word64
    ids = aux minBound
    aux :: Word64 -> Stream Word64
    aux i = Cons i (aux (succ i))

re :: Sem (NameIdGen ': r) a -> Sem (State NameIdGenState ': Reader NameIdGenCtx ': r) a
re = reinterpret2 $ \case
  FreshNameId -> do
    mid <- asks (^. nameIdGenCtxModuleId)
    Cons fresh rest <- gets (^. nameIdGenStateStream)
    put (NameIdGenState rest)
    return (NameId fresh mid)

runNameIdGenDefaultModule :: NameIdGenState -> Sem (NameIdGen ': r) a -> Sem r (NameIdGenState, a)
runNameIdGenDefaultModule s = runReader (NameIdGenCtx defaultModuleId) . runState s . re

runNameIdGen :: NameIdGenState -> ModuleId -> Sem (NameIdGen ': r) a -> Sem r (NameIdGenState, a)
runNameIdGen s c = runReader (NameIdGenCtx c) . runState s . re

runTopNameIdGen :: ModuleId -> Sem (NameIdGen ': r) a -> Sem r (NameIdGenState, a)
runTopNameIdGen = runNameIdGen iniNameIdState

evalTopNameIdGen :: ModuleId -> Sem (NameIdGen ': r) a -> Sem r a
evalTopNameIdGen mid = fmap snd . runTopNameIdGen mid
