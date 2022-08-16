module Juvix.Compiler.Core.Language.Type where

import Juvix.Compiler.Core.Language.Base

{-
- TyVar indicates a polymorphic type variable
- Atomic indicates an atom: e.g. an inductive type applied to type arguments
-}
data Type = TyVar Int | Universe Int | Atomic Atom | Fun Type Type

data Atom = Atom
  { _atomHead :: Name,
    _atomArgs :: [Type]
  }

makeLenses ''Atom
