module MiniJuvix.Syntax.Universe where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Fixity

newtype Universe = Universe
  { universeLevel :: Maybe Natural
  }
  deriving stock (Show, Eq, Ord)

instance HasAtomicity Universe where
  atomicity = const Atom
