{-# LANGUAGE UndecidableInstances #-}

module Commands.Dev.Anoma.Prove.Options.ProveArgTag where

import CommonOptions
import Prelude qualified

data ProveArgTag
  = ProveArgTagNat
  | ProveArgTagBase64
  | ProveArgTagBytes
  deriving stock (Eq, Bounded, Enum, Data)

instance Show ProveArgTag where
  show = \case
    ProveArgTagNat -> "nat"
    ProveArgTagBase64 -> "base64"
    ProveArgTagBytes -> "bytes"

type ProveArgType :: ProveArgTag -> GHCType
type family ProveArgType s = res where
  ProveArgType 'ProveArgTagNat = Natural
  ProveArgType 'ProveArgTagBase64 = AppPath File
  ProveArgType 'ProveArgTagBytes = AppPath File

$(genDefunSymbols [''ProveArgType])
$(genSingletons [''ProveArgTag])
