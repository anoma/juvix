module Juvix.Compiler.Concrete.Data.NameSignature.Base where

import Juvix.Compiler.Concrete.Data.Name
import Juvix.Prelude hiding (show)

data NameItem = NameItem {
  _nameItemSymbol :: Symbol,
  _nameItemIndex :: Int
  }
  deriving stock (Show)

data NameBlock = NameBlock
  { -- | Symbols map to themselves so we can retrive the location
    -- | NOTE the index is wrt to the block, not the whole signature.
    _nameBlock :: HashMap Symbol NameItem,
    _nameImplicit :: IsImplicit
  }
  deriving stock (Show)

-- | Two consecutive blocks should have different implicitness
newtype NameSignature = NameSignature
  { _nameSignatureArgs :: [NameBlock]
  }
  deriving stock (Show)

newtype RecordNameSignature = RecordNameSignature
  { _recordNames :: HashMap Symbol NameItem
  }
  deriving stock (Show)

makeLenses ''NameSignature
makeLenses ''RecordNameSignature
makeLenses ''NameBlock
makeLenses ''NameItem
