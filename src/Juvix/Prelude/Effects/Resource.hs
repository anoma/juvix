module Juvix.Prelude.Effects.Resource
  ( module Juvix.Prelude.Effects.Resource,
    module Effectful.Resource,
  )
where

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
  forall r a b.
  -- | Action to allocate a resource.
  Sem r a ->
  -- | Action to cleanup the resource. This will only be called if the
  -- "use" block fails.
  (a -> Sem r ()) ->
  -- | Action which uses the resource.
  (a -> Sem r b) ->
  Sem r b
bracketOnError alloc dealloc useRes = fst <$> generalBracketSem alloc dealloc' useRes
  where
    dealloc' :: a -> ExitCase b -> Sem r ()
    dealloc' a = \case
      ExitCaseSuccess {} -> return ()
      ExitCaseException {} -> dealloc a
      ExitCaseAbort {} -> dealloc a

generalBracketSem ::
  -- | aquire
  Sem r a ->
  -- | release
  (a -> ExitCase b -> Sem r c) ->
  -- | use
  (a -> Sem r b) ->
  Sem r (b, c)
generalBracketSem = generalBracket

finally ::
  (Member Resource r) =>
  -- | computation to run first
  Sem r a ->
  -- | computation to run afterward (even if an exception was raised)
  Sem r () ->
  Sem r a
finally act end = bracket (pure ()) (const end) (const act)

onException ::
  -- | computation to run first
  Sem r a ->
  -- | computation to run afterward if an exception was raised
  Sem r () ->
  Sem r a
onException act end = bracketOnError (pure ()) (const end) (const act)
