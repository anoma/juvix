{-# LANGUAGE UndecidableInstances #-}

module Juvix.Compiler.Core.Extra.Recursors.Classes where

import Data.Functor.Identity
import Data.Kind qualified as GHC
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Language.Base

type OverIdentity' :: GHC.Type -> GHC.Type
type family OverIdentity' t = res where
  OverIdentity' (a -> b) = a -> OverIdentity' b
  OverIdentity' leaf = Identity leaf

type OverIdentity :: GHC.Type -> GHC.Type
type family OverIdentity t = res where
  OverIdentity ((), b) = ((), OverIdentity' b)
  OverIdentity (BinderList b', b) = (BinderList b', OverIdentity' b)
  OverIdentity (Index, b) = (Index, OverIdentity' b)
  OverIdentity leaf = OverIdentity' leaf

class EmbedIdentity a where
  embedIden :: a -> OverIdentity a

class EmbedIdentity' a where
  embedIden' :: a -> OverIdentity' a

instance (EmbedIdentity' b) => EmbedIdentity' (a -> b) where
  embedIden' f = embedIden' . f

instance (EmbedIdentity' b) => EmbedIdentity ((), b) where
  embedIden (a, b) = (a, embedIden' b)

instance (EmbedIdentity' b) => EmbedIdentity (Index, b) where
  embedIden (a, b) = (a, embedIden' b)

instance (EmbedIdentity' b) => EmbedIdentity (BinderList b', b) where
  embedIden (a, b) = (a, embedIden' b)

instance (EmbedIdentity' b) => EmbedIdentity (a -> b) where
  embedIden a = embedIden' a
