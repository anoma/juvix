{-# LANGUAGE AllowAmbiguousTypes #-}

-- | This module requires AllowAmbiguousTypes, so it is separated from Base
module Juvix.Prelude.Tagged where

import Polysemy
import Polysemy.Tagged hiding (tag)
import Polysemy.Tagged qualified as Polysemy

-- | We rename it to ptag to avoid clashes with the commonly used `tag` identifier.
ptag :: forall k e r a. Member (Tagged k e) r => Sem (e ': r) a -> Sem r a
ptag = Polysemy.tag @k
