module Juvix.Data.Effect.Cache
  ( runCache,
    evalCache,
    cacheGet,
    cacheLookup,
    Cache,
  )
where

import Juvix.Prelude.Base

data Cache k v m a where
  CacheGet :: k -> Cache k v m v
  CacheLookup :: k -> Cache k v m (Maybe v)

makeSem ''Cache

-- | Run a 'Cache' effect purely.
runCache ::
  Hashable k =>
  (k -> Sem (Cache k v ': r) v) ->
  HashMap k v ->
  Sem (Cache k v ': r) a ->
  Sem r (HashMap k v, a)
runCache f c = runState c . re f
{-# INLINE runCache #-}

evalCache ::
  Hashable k =>
  (k -> Sem (Cache k v ': r) v) ->
  HashMap k v ->
  Sem (Cache k v ': r) a ->
  Sem r a
evalCache f c = fmap snd . runCache f c
{-# INLINE evalCache #-}

re ::
  forall k v r a.
  Hashable k =>
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
