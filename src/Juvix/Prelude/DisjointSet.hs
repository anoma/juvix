module Juvix.Prelude.DisjointSet
  ( module Juvix.Prelude.DisjointSet,
    module Data.DisjointSet,
  )
where

import Data.DisjointSet hiding (toLists)
import Data.DisjointSet qualified as DSet
import Juvix.Prelude.Base

toLists :: DisjointSet a -> [NonEmpty a]
toLists = map nonEmpty' . DSet.toLists
