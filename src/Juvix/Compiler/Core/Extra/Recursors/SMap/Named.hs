module Juvix.Compiler.Core.Extra.Recursors.SMap.Named where

import Data.Functor.Identity
import Juvix.Compiler.Core.Extra.Recursors.Base
import Juvix.Compiler.Core.Extra.Recursors.SMap

{-

The shallow mapping recursors are analogous to ordinary mapping recursors (see
Core/Extra/Recursors/Map/Named.hs) except that they don't go under binders.

-}

sdmapM :: (Monad m) => (Node -> m Node) -> Node -> m Node
sdmapM f = sdmapG (fmap Recur . f)

sumapM :: (Monad m) => (Node -> m Node) -> Node -> m Node
sumapM f = sumapG f

sdmapRM :: (Monad m) => (Node -> m Recur) -> Node -> m Node
sdmapRM f = sdmapG f

sdmapR :: (Node -> Recur) -> Node -> Node
sdmapR f = runIdentity . sdmapRM (embedIden f)

sdmap :: (Node -> Node) -> Node -> Node
sdmap f = runIdentity . sdmapM (embedIden f)

sumap :: (Node -> Node) -> Node -> Node
sumap f = runIdentity . sumapM (embedIden f)
