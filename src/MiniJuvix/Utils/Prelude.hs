module MiniJuvix.Utils.Prelude
  ( String,
    module Protolude,
    Semiring (..),
  )
where

--------------------------------------------------------------------------------

import Protolude hiding (Semiring (..))
import Prelude (String)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Semiring
--------------------------------------------------------------------------------

class Monoid m => Semiring m where
  {-# MINIMAL one, times #-}

  one :: m
  times :: m -> m -> m
