{-# LANGUAGE FunctionalDependencies #-}

module Juvix.Compiler.Core.Extra.Recursors.Generic.Base
  ( module Juvix.Compiler.Core.Extra.Recursors.Generic.Collector,
    module Juvix.Compiler.Core.Extra.Recursors.Generic.Recur,
    module Juvix.Compiler.Core.Language.Base,
    module Juvix.Compiler.Core.Extra.Recursors.Generic.Base,
  )
where

import Data.Functor.Identity
import Juvix.Compiler.Core.Extra.Recursors.Generic.Collector
import Juvix.Compiler.Core.Extra.Recursors.Generic.Recur
import Juvix.Compiler.Core.Language.Base

class IsNodeChild c b | c -> b where
  gBindersNum :: c -> Int
  gBinders :: c -> [b]

class IsNodeDetails d c | d -> c where
  gChildren :: d -> [c]

class (IsNodeDetails d c, IsNodeChild c b) => IsNode n d c b | n -> d c b where
  gDestruct :: n -> d
  gReassemble :: d -> [n] -> n
  gChild :: c -> n

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
