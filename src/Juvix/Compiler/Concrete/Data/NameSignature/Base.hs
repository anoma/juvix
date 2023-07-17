module Juvix.Compiler.Concrete.Data.NameSignature.Base where

import Juvix.Compiler.Concrete.Data.Name
import Juvix.Prelude hiding (show)

data NameBlock = NameBlock
  { -- | Symbols map to themselves so we can retrive the location
    -- | NOTE the index is wrt to the block, not the whole signature.
    _nameBlock :: HashMap Symbol (Symbol, Int),
    _nameImplicit :: IsImplicit
  }
  deriving stock (Show)

-- | Two consecutive blocks should have different implicitness
newtype NameSignature = NameSignature
  { _nameSignatureArgs :: [NameBlock]
  }
  deriving stock (Show)

makeLenses ''NameSignature
makeLenses ''NameBlock
