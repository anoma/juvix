module Juvix.Compiler.Core.Extra.Recursors.SFold.Named where

import Data.Functor.Identity
import Juvix.Compiler.Core.Extra.Recursors.Base
import Juvix.Compiler.Core.Extra.Recursors.SFold

{-

The shallow folding recursors are analogous to general folding recursors (see
Core/Extra/Recursors/Fold/Named.hs) except that they don't go under binders.

-}

sfoldA :: (Applicative f) => (a -> [a] -> a) -> (Node -> f a) -> Node -> f a
sfoldA uplus f = sfoldG uplus f

swalk :: (Applicative f) => (Node -> f ()) -> Node -> f ()
swalk = sfoldA (foldr mappend)

sfold :: (a -> [a] -> a) -> (Node -> a) -> Node -> a
sfold uplus f = runIdentity . sfoldA uplus (return . f)

sgather :: (a -> Node -> a) -> a -> Node -> a
sgather f acc = run . execState acc . swalk (\n' -> modify' (`f` n'))
