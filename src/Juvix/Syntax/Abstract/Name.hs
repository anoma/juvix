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
import Text.Show qualified as Show

data Name = Name
  { _nameText :: Text,
    _nameId :: NameId,
    _nameKind :: NameKind,
    _namePretty :: Text, -- How to print this name in error messages
    _nameLoc :: Interval
  }

instance Show Name where
  show Name {..} = show . unpack $ _nameText

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
    pretty (n ^. namePretty)
      <> "@"
      <> pretty (n ^. nameId)

prettyName :: HasNameKindAnn a => Bool -> Name -> Doc a
prettyName showNameId n =
  annotate
    (annNameKind (n ^. nameKind))
    (pretty (n ^. namePretty) <?> uid)
  where
    uid
      | showNameId = Just ("@" <> pretty (n ^. nameId))
      | otherwise = Nothing

type FunctionName = Name

type ConstructorName = Name

type AxiomName = Name

type VarName = Name

type ConstrName = Name

type InductiveName = Name
