module MiniJuvix.Syntax.Concrete.Scoped.Name
  ( module MiniJuvix.Syntax.Concrete.Scoped.Name,
    module MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind,
    module MiniJuvix.Syntax.NameId,
  )
where

import Data.Stream (Stream (Cons))
import Lens.Micro.Platform
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Loc
import MiniJuvix.Syntax.Concrete.Name qualified as C
import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import MiniJuvix.Syntax.Concrete.Scoped.VisibilityAnn
import MiniJuvix.Syntax.Fixity qualified as C
import MiniJuvix.Syntax.NameId

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

data IsConcrete = NotConcrete | Concrete

$(genSingletons [''IsConcrete])

data AbsModulePath = AbsModulePath
  { absTopModulePath :: C.TopModulePath,
    absLocalPath :: [C.Symbol]
  }
  deriving stock (Show, Eq, Generic)

topModulePathToAbsPath :: C.TopModulePath -> AbsModulePath
topModulePathToAbsPath p = AbsModulePath p []

instance Hashable AbsModulePath

-- | Tells whether the first argument is an immediate child of the second argument.
-- In other words, tells whether the first argument is a local module of the second.
isChildOf :: AbsModulePath -> AbsModulePath -> Bool
isChildOf child parent
  | null (absLocalPath child) = False
  | otherwise =
      init (absLocalPath child) == absLocalPath parent
        && absTopModulePath child == absTopModulePath parent

-- | Appends a local path to the absolute path
-- e.g. TopMod.Local <.> Inner == TopMod.Local.Inner
(<.>) :: AbsModulePath -> C.Symbol -> AbsModulePath
absP <.> localMod = absP {absLocalPath = absLocalPath absP ++ [localMod]}

allNameIds :: Stream NameId
allNameIds = NameId <$> ids
  where
    ids :: Stream Word64
    ids = aux minBound
    aux i = Cons i (aux (succ i))

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
  getNameKind = _nameKind

instance HasLoc n => HasLoc (Name' n) where
  getLoc = getLoc . _nameConcrete

hasFixity :: Name' s -> Bool
hasFixity Name' {..} = isJust _nameFixity

isConstructor :: Name' s -> Bool
isConstructor Name' {..} = case _nameKind of
  KNameConstructor {} -> True
  _ -> False

fromQualifiedName :: C.QualifiedName -> C.Symbol
fromQualifiedName (C.QualifiedName _ s) = s

topModulePathName :: TopModulePath -> Symbol
topModulePathName = over nameConcrete C._modulePathName

symbolText :: Symbol -> Text
symbolText = C._symbolText . _nameConcrete

unqualifiedSymbol :: Symbol -> Name
unqualifiedSymbol = over nameConcrete C.NameUnqualified

nameUnqualify :: Name -> Symbol
nameUnqualify Name' {..} = Name' {_nameConcrete = unqual, ..}
  where
    unqual = case _nameConcrete of
      C.NameUnqualified s -> s
      C.NameQualified q -> fromQualifiedName q

instance Eq (Name' n) where
  (==) = (==) `on` _nameId

instance Ord (Name' n) where
  compare = compare `on` _nameId

instance Hashable (Name' n) where
  hashWithSalt salt = hashWithSalt salt . _nameId
