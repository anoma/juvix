-- | Visit every key at most once
module Juvix.Data.Effect.Visit
  ( runVisit,
    runVisit',
    evalVisit,
    Visit,
    visit,
  )
where

import Data.HashSet qualified as HashSet
import Juvix.Prelude.Base

data Visit k m a where
  Visit :: k -> Visit k m ()

makeSem ''Visit

-- | Run a 'Visit' effect purely.
runVisit' ::
  Hashable k =>
  (k -> Sem (Visit k ': r) ()) ->
  HashSet k ->
  Sem (Visit k ': r) a ->
  Sem r (HashSet k, a)
runVisit' f c = runState c . re f
{-# INLINE runVisit' #-}

runVisit ::
  Hashable k =>
  (k -> Sem (Visit k ': r) ()) ->
  Sem (Visit k ': r) a ->
  Sem r (HashSet k, a)
runVisit f = runVisit' f mempty
{-# INLINE runVisit #-}

evalVisit ::
  Hashable k =>
  (k -> Sem (Visit k ': r) ()) ->
  Sem (Visit k ': r) a ->
  Sem r a
evalVisit f = fmap snd . runVisit f
{-# INLINE evalVisit #-}

re ::
  forall k r a.
  Hashable k =>
  (k -> Sem (Visit k ': r) ()) ->
  Sem (Visit k ': r) a ->
  Sem (State (HashSet k) ': r) a
re vis = reinterpret $ \case
  Visit k ->
    unlessM (HashSet.member k <$> get @(HashSet k)) $ do
      modify' (HashSet.insert k)
      re vis (vis k)
{-# INLINE re #-}
