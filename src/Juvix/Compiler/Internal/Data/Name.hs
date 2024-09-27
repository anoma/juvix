module Juvix.Compiler.Internal.Data.Name
  ( module Juvix.Compiler.Internal.Data.Name,
    module Juvix.Data.NameKind,
    module Juvix.Data.NameId,
    module Juvix.Data.Fixity,
  )
where

import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Data.Fixity
import Juvix.Data.NameId
import Juvix.Data.NameKind
import Juvix.Extra.Serialize
import Juvix.Prelude
import Juvix.Prelude.Pretty

data Name = Name
  { _nameText :: Text,
    _nameId :: NameId,
    _nameKind :: NameKind,
    _nameKindPretty :: NameKind,
    -- |  How to print this name in error messages
    _namePretty :: Text,
    _nameLoc :: Interval,
    _nameFixity :: Maybe Fixity
  }
  deriving stock (Show, Data, Generic)

makeLenses ''Name

instance Serialize Name

instance NFData Name

varFromHole :: Hole -> VarName
varFromHole h =
  Name
    { _nameText = pp,
      _nameKind = KNameLocal,
      _nameKindPretty = KNameLocal,
      _namePretty = pp,
      _nameLoc = getLoc h,
      _nameId = h ^. holeId,
      _nameFixity = Nothing
    }
  where
    pp :: Text = "_ω"

varFromWildcard :: (Members '[NameIdGen] r) => Wildcard -> Sem r VarName
varFromWildcard w = do
  _nameId <- freshNameId
  let _nameText :: Text = "_ω"
      _nameKind = KNameLocal
      _nameKindPretty = KNameLocal
      _namePretty = _nameText
      _nameLoc = getLoc w
      _nameFixity :: Maybe Fixity
      _nameFixity = Nothing
  return Name {..}

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
  getNameKindPretty = (^. nameKindPretty)

instance Pretty Name where
  pretty n = pretty (n ^. namePretty)

addNameIdTag :: Bool -> NameId -> Doc a -> Doc a
addNameIdTag showNameId nid
  | showNameId = (<> ("@" <> pretty nid))
  | otherwise = id

prettyName :: (HasNameKindAnn a) => Bool -> Name -> Doc a
prettyName showNameId n =
  annotate
    (annNameKind (n ^. nameKindPretty))
    (addNameIdTag showNameId (n ^. nameId) (pretty (n ^. namePretty)))

type FunctionName = Name

type ConstructorName = Name

type AxiomName = Name

type VarName = Name

type ConstrName = Name

type InductiveName = Name

type InductiveId = NameId

type InductiveParam = Name

fromConcreteSymbol :: Interval -> S.Symbol -> Name
fromConcreteSymbol loc s = fromConcreteSymbolPretty loc (S.symbolText s) s

fromConcreteSymbolPretty :: Interval -> Text -> S.Symbol -> Name
fromConcreteSymbolPretty loc pp s =
  Name
    { _nameText = S.symbolText s,
      _nameId = s ^. S.nameId,
      _nameKind = getNameKind s,
      _nameKindPretty = getNameKindPretty s,
      _namePretty = pp,
      _nameLoc = loc,
      _nameFixity = s ^. S.nameFixity
    }
