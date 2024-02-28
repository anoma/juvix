module Juvix.Prelude.Effects.TheirResource where

import Effectful.Resource hiding (register)
import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Base

bracket ::
  forall r a b.
  (Member Resource r) =>
  Sem r a ->
  (a -> Sem r ()) ->
  (a -> Sem r b) ->
  Sem r b
bracket alloc free inside = do
  (_releaseKey, resource) <- allocateEff alloc free
  inside resource

bracketOnError ::
  (Member Resource r) =>
  -- | Action to allocate a resource.
  Sem r a ->
  -- | Action to cleanup the resource. This will only be called if the
  -- "use" block fails.
  (a -> Sem r c) ->
  -- | Action which uses the resource.
  (a -> Sem r b) ->
  Sem r b
bracketOnError alloc dealloc useRes = undefined
