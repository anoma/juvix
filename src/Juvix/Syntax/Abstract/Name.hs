module Juvix.Syntax.Abstract.Name
  ( module Juvix.Syntax.Abstract.Name,
    module Juvix.Syntax.Concrete.Scoped.Name.NameKind,
    module Juvix.Syntax.NameId,
    module Juvix.Syntax.Fixity,
  )
where

import Juvix.Prelude
import Juvix.Prelude.Pretty
import Juvix.Syntax.Concrete.Scoped.Name.NameKind
import Juvix.Syntax.Fixity
import Juvix.Syntax.NameId

data Name = Name
  { _nameText :: Text,
    _nameId :: NameId,
    _nameKind :: NameKind,
    _nameLoc :: Interval
  }
  deriving stock (Show)

makeLenses ''Name

instance HasAtomicity Name where
  atomicity = const Atom

instance HasLoc Name where
  getLoc = (^. nameLoc)

instance Eq Name where
  (==) = (==) `on` (^. nameId)

instance Ord Name where
  compare = compare `on` (^. nameId)

instance Hashable Name where
  hashWithSalt salt = hashWithSalt salt . (^. nameId)

instance HasNameKind Name where
  getNameKind = (^. nameKind)

instance Pretty Name where
  pretty n =
    pretty (n ^. nameText)
      <> "@"
      <> pretty (n ^. nameId)

type FunctionName = Name

type ConstructorName = Name

type AxiomName = Name

type VarName = Name

type ConstrName = Name

type InductiveName = Name
