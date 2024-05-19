{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Juvix.Data.Effect.Cache
  ( runCache,
    evalCache,
    evalCacheEmpty,
    runCacheEmpty,
    cacheGet,
    cacheLookup,
    cacheAsk,
    evalSingletonCache,
    cacheSingletonGet,
    Cache,
    SCache,
  )
where

import Juvix.Prelude.Base

data Cache (k :: GHCType) (v :: GHCType) :: Effect where
  CacheGet :: k -> Cache k v m v
  CacheLookup :: k -> Cache k v m (Maybe v)
  CacheAsk :: Cache k v m (HashMap k v)

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
      CacheAsk -> getShared @(HashMap k v)
      CacheGet k -> do
        mv <- getsShared @(HashMap k v) (^. at k)
        case mv of
          Nothing -> do
            x <- re f (f k)
            modifyShared @(HashMap k v) (set (at k) (Just x))
            return x
          Just v -> return v
