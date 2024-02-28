module Juvix.Prelude.Effects.Bracket
  ( module Juvix.Prelude.Effects.Bracket,
    module Juvix.Prelude.Effects.Bracket.Base,
  )
where

import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Base
import Juvix.Prelude.Effects.Bracket.Base

bracket ::
  forall r a b.
  (Member EmbedIO r) =>
  Sem r a ->
  (a -> Sem r ()) ->
  (a -> Sem r b) ->
  Sem r b
bracket alloc dealloc useRes = fst <$> generalBracketSem alloc dealloc' useRes
  where
    dealloc' a = const (dealloc a)

bracketOnError ::
  forall r a b.
  (Member EmbedIO r) =>
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

finally ::
  (Member EmbedIO r) =>
  -- | computation to run first
  Sem r a ->
  -- | computation to run afterward (even if an exception was raised)
  Sem r () ->
  Sem r a
finally act end = bracket (pure ()) (const end) (const act)

onException ::
  (Member EmbedIO r) =>
  -- | computation to run first
  Sem r a ->
  -- | computation to run afterward if an exception was raised
  Sem r () ->
  Sem r a
onException act end = bracketOnError (pure ()) (const end) (const act)
