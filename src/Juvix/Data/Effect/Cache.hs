{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Data.Effect.Cache
  ( runCache,
    evalCache,
    evalCacheEmpty,
    runCacheEmpty,
    cacheGet,
    cacheGetResult,
    cacheLookup,
    evalSingletonCache,
    cacheSingletonGet,
    CacheResult (..),
    cacheResultHit,
    cacheResult,
    Cache,
    SCache,
  )
where

import Juvix.Prelude.Base

data CacheResult a = CacheResult
  { _cacheResultHit :: Bool,
    _cacheResult :: a
  }
  deriving stock (Show, Eq)

makeLenses ''CacheResult

data Cache (k :: GHCType) (v :: GHCType) :: Effect where
  CacheGetResult :: k -> Cache k v m (CacheResult v)
  CacheLookup :: k -> Cache k v m (Maybe v)

makeSem ''Cache

-- | Singleton cache
type SCache = Cache ()

-- | Run a 'Cache' effect purely.
runCache ::
  forall k v r a.
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) v) ->
  HashMap k v ->
  Sem (Cache k v ': r) a ->
  Sem r (HashMap k v, a)
runCache f c = runStateShared c . re f

evalCache ::
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) v) ->
  HashMap k v ->
  Sem (Cache k v ': r) a ->
  Sem r a
evalCache f c = fmap snd . runCache f c
{-# INLINE evalCache #-}

evalCacheEmpty ::
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) v) ->
  Sem (Cache k v ': r) a ->
  Sem r a
evalCacheEmpty f = evalCache f mempty
{-# INLINE evalCacheEmpty #-}

runCacheEmpty ::
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) v) ->
  Sem (Cache k v ': r) a ->
  Sem r (HashMap k v, a)
runCacheEmpty f = runCache f mempty
{-# INLINE runCacheEmpty #-}

cacheSingletonGet :: (Members '[SCache v] r) => Sem r v
cacheSingletonGet = cacheGet ()

evalSingletonCache ::
  Sem (SCache v ': r) v ->
  Sem (SCache v ': r) a ->
  Sem r a
evalSingletonCache f c = evalCacheEmpty @() (const f) c
{-# INLINE evalSingletonCache #-}

cacheGet ::
  forall k v r.
  (Member (Cache k v) r) =>
  k ->
  Sem r v
cacheGet = fmap (^. cacheResult) . cacheGetResult

re ::
  forall k v r a.
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) v) ->
  Sem (Cache k v ': r) a ->
  Sem (SharedState (HashMap k v) ': r) a
re f =
  interpretTop $
    \case
      CacheLookup k -> getsShared @(HashMap k v) (^. at k)
      CacheGetResult k -> do
        mv <- getsShared @(HashMap k v) (^. at k)
        case mv of
          Nothing -> do
            _cacheResult <- re f (f k)
            modifyShared @(HashMap k v) (set (at k) (Just _cacheResult))
            return
              CacheResult
                { _cacheResultHit = False,
                  _cacheResult
                }
          Just _cacheResult ->
            return
              CacheResult
                { _cacheResultHit = True,
                  _cacheResult
                }
