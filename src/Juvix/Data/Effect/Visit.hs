-- | Visit every key at most once
module Juvix.Data.Effect.Visit
  ( runVisit,
    runVisitEmpty,
    evalVisit,
    evalVisitEmpty,
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
runVisit ::
  (Hashable k) =>
  (k -> Sem (Visit k ': r) ()) ->
  HashSet k ->
  Sem (Visit k ': r) a ->
  Sem r (HashSet k, a)
runVisit f c = runState c . re f
{-# INLINE runVisit #-}

runVisitEmpty ::
  (Hashable k) =>
  (k -> Sem (Visit k ': r) ()) ->
  Sem (Visit k ': r) a ->
  Sem r (HashSet k, a)
runVisitEmpty f = runVisit f mempty
{-# INLINE runVisitEmpty #-}

evalVisitEmpty ::
  (Hashable k) =>
  (k -> Sem (Visit k ': r) ()) ->
  Sem (Visit k ': r) a ->
  Sem r a
evalVisitEmpty f = fmap snd . runVisitEmpty f
{-# INLINE evalVisitEmpty #-}

evalVisit ::
  (Hashable k) =>
  (k -> Sem (Visit k ': r) ()) ->
  HashSet k ->
  Sem (Visit k ': r) a ->
  Sem r a
evalVisit f c = fmap snd . runVisit f c
{-# INLINE evalVisit #-}

re ::
  forall k r a.
  (Hashable k) =>
  (k -> Sem (Visit k ': r) ()) ->
  Sem (Visit k ': r) a ->
  Sem (State (HashSet k) ': r) a
re vis = reinterpret $ \case
  Visit k ->
    unlessM (HashSet.member k <$> get @(HashSet k)) $ do
      modify' (HashSet.insert k)
      re vis (vis k)
{-# INLINE re #-}
