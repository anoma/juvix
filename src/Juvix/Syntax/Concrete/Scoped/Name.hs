module Juvix.Syntax.Concrete.Scoped.Name
  ( module Juvix.Syntax.Concrete.Scoped.Name,
    module Juvix.Syntax.Concrete.Scoped.Name.NameKind,
    module Juvix.Syntax.NameId,
  )
where

import Juvix.Prelude
import Juvix.Syntax.Concrete.Name qualified as C
import Juvix.Syntax.Concrete.Scoped.Name.NameKind
import Juvix.Syntax.Concrete.Scoped.VisibilityAnn
import Juvix.Syntax.Fixity qualified as C
import Juvix.Syntax.NameId

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

data IsConcrete = NotConcrete | Concrete

$(genSingletons [''IsConcrete])

data AbsModulePath = AbsModulePath
  { _absTopModulePath :: C.TopModulePath,
    _absLocalPath :: [C.Symbol]
  }
  deriving stock (Show, Eq, Generic)

makeLenses ''AbsModulePath

instance HasLoc AbsModulePath where
  getLoc a = getLoc (a ^. absTopModulePath)

topModulePathToAbsPath :: C.TopModulePath -> AbsModulePath
topModulePathToAbsPath p = AbsModulePath p []

instance Hashable AbsModulePath

-- | Tells whether the first argument is an immediate child of the second argument.
-- In other words, tells whether the first argument is a local module of the second.
isChildOf :: AbsModulePath -> AbsModulePath -> Bool
isChildOf child parent
  | null (child ^. absLocalPath) = False
  | otherwise =
      init (child ^. absLocalPath) == parent ^. absLocalPath
        && child ^. absTopModulePath == parent ^. absTopModulePath

-- | Appends a local path to the absolute path
-- e.g. TopMod.Local <.> Inner == TopMod.Local.Inner
(<.>) :: AbsModulePath -> C.Symbol -> AbsModulePath
absP <.> localMod = absP {_absLocalPath = absP ^. absLocalPath ++ [localMod]}

-- | Why a symbol is in scope.
data WhyInScope
  = -- | Inherited from the parent module.
    BecauseInherited WhyInScope
  | -- | Opened or imported in this module.
    BecauseImportedOpened
  | -- | Defined in this module.
    BecauseDefined
  deriving stock (Show)

type Name = Name' C.Name

type Symbol = Name' C.Symbol

type TopModulePath = Name' C.TopModulePath

type ModuleNameId = NameId

data Name' n = Name'
  { _nameConcrete :: n,
    _nameId :: NameId,
    _nameDefined :: Interval,
    _nameKind :: NameKind,
    _nameDefinedIn :: AbsModulePath,
    _nameFixity :: Maybe C.Fixity,
    _nameWhyInScope :: WhyInScope,
    _nameVisibilityAnn :: VisibilityAnn,
    -- | The textual representation of the name at the binding site
    _nameVerbatim :: Text
  }
  deriving stock (Show)

makeLenses ''Name'

instance HasNameKind (Name' n) where
  getNameKind = (^. nameKind)

instance HasLoc n => HasLoc (Name' n) where
  getLoc = getLoc . (^. nameConcrete)

data AName = forall c.
  HasLoc c =>
  AName
  {_aName :: Name' c}

instance HasLoc AName where
  getLoc (AName c) = getLoc c

instance HasNameKind AName where
  getNameKind (AName c) = getNameKind c

hasFixity :: Name' s -> Bool
hasFixity n = isJust (n ^. nameFixity)

isConstructor :: Name' s -> Bool
isConstructor n = case n ^. nameKind of
  KNameConstructor {} -> True
  _ -> False

fromQualifiedName :: C.QualifiedName -> C.Symbol
fromQualifiedName (C.QualifiedName _ s) = s

topModulePathName :: TopModulePath -> Symbol
topModulePathName = over nameConcrete (^. C.modulePathName)

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
