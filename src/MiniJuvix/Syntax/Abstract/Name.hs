module MiniJuvix.Syntax.Abstract.Name
  ( module MiniJuvix.Syntax.Abstract.Name,
    module MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind,
    module MiniJuvix.Syntax.NameId,
    module MiniJuvix.Syntax.Fixity,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import MiniJuvix.Syntax.Fixity
import MiniJuvix.Syntax.NameId

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
