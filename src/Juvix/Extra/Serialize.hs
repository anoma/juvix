{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted flags" #-}
module Juvix.Extra.Serialize
  ( module S,
  )
where

import Data.Serialize as S
import Juvix.Prelude.Base
import Juvix.Prelude.Path

instance Serialize (Path Abs File)

instance Serialize Text where
  put txt = S.put (unpack txt)

  get = pack <$> S.get

instance (Serialize a) => Serialize (NonEmpty a)
