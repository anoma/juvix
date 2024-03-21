{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Juvix.Prelude.Effects.Bracket.Base where

import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Base

-- | We have the `EmbedIO` constraint because the `MonadMask` instance for Sem
-- does side effects.
generalBracketSem ::
  (Member EmbedIO r) =>
  -- | aquire
  Sem r a ->
  -- | release
  (a -> ExitCase b -> Sem r c) ->
  -- | use
  (a -> Sem r b) ->
  Sem r (b, c)
generalBracketSem = generalBracket
