module Juvix.Compiler.Concrete.Data.InfoTable where

import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

newtype FunctionInfo = FunctionInfo (FunctionDef 'Scoped)
  deriving stock (Eq, Show)

data ConstructorInfo = ConstructorInfo
  { _constructorInfoDef :: ConstructorDef 'Scoped,
    _constructorInfoTypeName :: S.Symbol
  }
  deriving stock (Eq, Show)

newtype AxiomInfo = AxiomInfo
  { _axiomInfoDef :: AxiomDef 'Scoped
  }
  deriving stock (Eq, Show)

newtype InductiveInfo = InductiveInfo
  { _inductiveInfoDef :: InductiveDef 'Scoped
  }
  deriving stock (Eq, Show)

type DocTable = HashMap NameId (Judoc 'Scoped)

data InfoTable = InfoTable
  { _infoConstructors :: HashMap S.NameId ConstructorInfo,
    _infoModules :: HashMap S.TopModulePath (Module 'Scoped 'ModuleTop),
    _infoAxioms :: HashMap S.NameId AxiomInfo,
    _infoInductives :: HashMap S.NameId InductiveInfo,
    _infoFunctions :: HashMap S.NameId FunctionInfo,
    _infoFixities :: HashMap S.NameId FixityDef,
    _infoPriorities :: IntSet,
    _infoPrecedenceGraph :: HashMap S.NameId (HashSet S.NameId)
  }

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _infoConstructors = mempty,
      _infoAxioms = mempty,
      _infoModules = mempty,
      _infoInductives = mempty,
      _infoFunctions = mempty,
      _infoFixities = mempty,
      _infoPriorities = mempty,
      _infoPrecedenceGraph = mempty
    }

makeLenses ''InfoTable
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''AxiomInfo

functionInfoDoc :: Lens' FunctionInfo (Maybe (Judoc 'Scoped))
functionInfoDoc f = \case
  FunctionInfo i -> do
    i' <- traverseOf signDoc f i
    pure (FunctionInfo i')

instance HasLoc FunctionInfo where
  getLoc = \case
    FunctionInfo f -> getLoc f
