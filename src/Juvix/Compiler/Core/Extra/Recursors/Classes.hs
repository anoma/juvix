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

embedIden1 :: (a -> b) -> a -> Identity b
embedIden1 f = Identity . f

embedIden2 :: (a -> b -> c) -> a -> b -> Identity c
embedIden2 f = embedIden1 . f

embedIden3 :: (a -> b -> c -> d) -> a -> b -> c -> Identity d
embedIden3 f = embedIden2 . f

embedIdenP1 :: (p, a -> b) -> (p, a -> Identity b)
embedIdenP1 = second embedIden1

embedIdenP2 :: (p, a -> b -> c) -> (p, a -> b -> Identity c)
embedIdenP2 = second embedIden2

embedIdenP3 :: (p, a -> b -> c -> d) -> (p, a -> b -> c -> Identity d)
embedIdenP3 = second embedIden3
