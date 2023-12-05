module Juvix.Data.Effect.Cache
  ( runCache,
    evalCache,
    evalCacheEmpty,
    runCacheEmpty,
    cacheGet,
    cacheLookup,
    evalSingletonCache,
    cacheSingletonGet,
    Cache,
    SCache,
  )
where

import Juvix.Prelude.Base

data Cache k v m a where
  CacheGet :: k -> Cache k v m v
  CacheLookup :: k -> Cache k v m (Maybe v)

-- | Singleton cache
type SCache = Cache ()

makeSem ''Cache

-- | Run a 'Cache' effect purely.
runCache ::
  (Hashable k) =>
  (k -> Sem (Cache k v ': r) v) ->
  HashMap k v ->
  Sem (Cache k v ': r) a ->
  Sem r (HashMap k v, a)
runCache f c = runState c . re f
{-# INLINE runCache #-}

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
  Sem (State (HashMap k v) ': r) a
re f = reinterpret $ \case
  CacheLookup k -> gets @(HashMap k v) (^. at k)
  CacheGet k -> do
    mv <- gets @(HashMap k v) (^. at k)
    case mv of
      Nothing -> do
        x <- re f (f k)
        modify' @(HashMap k v) (set (at k) (Just x))
        return x
      Just v -> return v
{-# INLINE re #-}
