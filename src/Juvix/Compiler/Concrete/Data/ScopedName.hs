module Juvix.Compiler.Concrete.Data.ScopedName
  ( module Juvix.Compiler.Concrete.Data.ScopedName,
    module Juvix.Compiler.Concrete.Data.IsConcrete,
    module Juvix.Data.NameKind,
    module Juvix.Data.NameId,
  )
where

import Juvix.Compiler.Concrete.Data.IsConcrete
import Juvix.Compiler.Concrete.Data.Name qualified as C
import Juvix.Compiler.Concrete.Data.VisibilityAnn
import Juvix.Data.Fixity qualified as C
import Juvix.Data.IteratorInfo
import Juvix.Data.NameId
import Juvix.Data.NameKind
import Juvix.Extra.Serialize
import Juvix.Prelude
import Juvix.Prelude.Pretty

-- | Why a symbol is in scope.
data WhyInScope
  = -- | Inherited from the parent module.
    BecauseInherited WhyInScope
  | -- | Opened or imported in this module.
    BecauseImportedOpened
  | -- | Defined in this module.
    BecauseDefined
  deriving stock (Eq, Show, Generic)

instance Serialize WhyInScope

instance NFData WhyInScope

type Name = Name' C.Name

type Symbol = Name' C.Symbol

type TopModulePath = Name' C.TopModulePath

data Name' n = Name'
  { _nameConcrete :: n,
    _nameId :: NameId,
    _nameDefined :: Interval,
    _nameKind :: NameKind,
    -- | Used to display sensitive colors for builtins. It the name is not a
    -- builtin, then _nameKind == _nameKindPretty
    _nameKindPretty :: NameKind,
    -- | True when the name is defined in a top definition (including top
    -- definitions in local modules).
    _nameTop :: Bool,
    _nameDefinedIn :: C.AbsModulePath,
    _nameFixity :: Maybe C.Fixity,
    _nameIterator :: Maybe IteratorInfo,
    _nameWhyInScope :: WhyInScope,
    _nameVisibilityAnn :: VisibilityAnn,
    -- | The textual representation of the name at the binding site
    _nameVerbatim :: Text
  }
  deriving stock (Show, Generic)

instance Serialize Name

instance NFData Name

instance Serialize Symbol

instance NFData Symbol

instance Serialize TopModulePath

instance NFData TopModulePath

-- | For highlighting
data AName = AName
  { _anameLoc :: Interval,
    _anameDefinedLoc :: Interval,
    _anameKindPretty :: NameKind,
    _anameIsTop :: Bool,
    _anameDocId :: NameId,
    _anameVerbatim :: Text
  }
  deriving stock (Generic)

instance Serialize AName

instance NFData AName

makeLenses ''Name'
makeLenses ''AName

anameFromName :: (HasLoc c) => Bool -> Name' c -> AName
anameFromName isTop n =
  AName
    { _anameLoc = getLoc n,
      _anameDefinedLoc = n ^. nameDefined,
      _anameKindPretty = getNameKindPretty n,
      _anameDocId = n ^. nameId,
      _anameIsTop = isTop,
      _anameVerbatim = n ^. nameVerbatim
    }

instance HasNameKind (Name' n) where
  getNameKind = (^. nameKind)
  getNameKindPretty = (^. nameKindPretty)

instance (HasLoc n) => HasLoc (Name' n) where
  getLoc = getLoc . (^. nameConcrete)

instance (Pretty a) => Pretty (Name' a) where
  pretty = pretty . (^. nameConcrete)

instance HasLoc AName where
  getLoc = (^. anameLoc)

instance HasNameKind AName where
  getNameKind = (^. anameKindPretty)
  getNameKindPretty = (^. anameKindPretty)

hasFixity :: Name' s -> Bool
hasFixity n = case n ^. nameFixity of
  Just Fixity {..} -> _fixityArity /= OpNone
  Nothing -> False

isConstructor :: Name' s -> Bool
isConstructor n = case n ^. nameKind of
  KNameConstructor {} -> True
  _ -> False

fromQualifiedName :: C.QualifiedName -> C.Symbol
fromQualifiedName (C.QualifiedName _ s) = s

topModulePathSymbol :: TopModulePath -> Symbol
topModulePathSymbol = over nameConcrete (^. C.modulePathName)

topModulePathName :: TopModulePath -> Name
topModulePathName = over nameConcrete C.topModulePathToName

symbolText :: Symbol -> Text
symbolText s = s ^. nameConcrete . C.symbolText

unqualifiedSymbol :: Symbol -> Name
unqualifiedSymbol = over nameConcrete C.NameUnqualified

nameUnqualify :: Name -> Symbol
nameUnqualify Name' {..} = Name' {_nameConcrete = unqual, ..}
  where
    unqual = case _nameConcrete of
      C.NameUnqualified s -> s
      C.NameQualified q -> fromQualifiedName q

nameUnqualifiedText :: Name -> Text
nameUnqualifiedText = symbolText . nameUnqualify

instance Eq (Name' n) where
  (==) = (==) `on` (^. nameId)

instance Ord (Name' n) where
  compare = compare `on` (^. nameId)

instance Hashable (Name' n) where
  hashWithSalt salt = hashWithSalt salt . (^. nameId)
