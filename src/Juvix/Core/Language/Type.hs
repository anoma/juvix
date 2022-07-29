module Juvix.Core.Language.Type where

import Juvix.Core.Language.Base

data Type = Atomic Atom | Fun Type Type | Universe

data Atom = Atom
  { _atomHead :: Name,
    _atomArgs :: [Type]
  }

makeLenses ''Atom
