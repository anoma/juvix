{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted flags" #-}
module Juvix.Extra.Serialize
  ( module S,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Serialize as S
import Juvix.Prelude.Base
import Juvix.Prelude.Path

instance Serialize (Path Abs File)

instance Serialize Text where
  put txt = S.put (unpack txt)

  get = pack <$> S.get

instance (Serialize a) => Serialize (NonEmpty a)

instance (Hashable k, Serialize k, Serialize a) => Serialize (HashMap k a) where
  put m = S.put (HashMap.toList m)

  get = HashMap.fromList <$> S.get

instance (Hashable a, Serialize a) => Serialize (HashSet a) where
  put s = S.put (HashSet.toList s)

  get = HashSet.fromList <$> S.get
