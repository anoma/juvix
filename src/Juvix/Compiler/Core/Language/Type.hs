module Juvix.Compiler.Core.Language.Type where

import Juvix.Compiler.Core.Language.Base

-- Star (*) allows to specify the type partially, e.g.: * -> * -> *.
data Type = Atomic Atom | Fun Type Type | Universe | Star

data Atom = Atom
  { _atomHead :: Name,
    _atomArgs :: [Type]
  }

makeLenses ''Atom
