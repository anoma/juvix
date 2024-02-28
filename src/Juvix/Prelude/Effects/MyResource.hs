module Juvix.Prelude.Effects.MyResource where

import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Base

data Resource m a where
  -- | Allocate a resource, use it, and clean it up afterwards.
  Bracket ::
    m a ->
    -- Action to allocate a resource.
    (a -> m c) ->
    -- Action to cleanup the resource. This is guaranteed to be
    -- called.
    (a -> m b) ->
    -- Action which uses the resource.
    Resource m b
  -- | Allocate a resource, use it, and clean it up afterwards if an error occurred.
  BracketOnError ::
    m a ->
    -- Action to allocate a resource.
    (a -> m c) ->
    -- Action to cleanup the resource. This will only be called if the
    -- "use" block fails.
    (a -> m b) ->
    -- Action which uses the resource.
    Resource m b

makeSem ''Resource

runResource ::
  (Member EmbedIO r) =>
  Sem (Resource ': r) a ->
  Sem r a
runResource = undefined
