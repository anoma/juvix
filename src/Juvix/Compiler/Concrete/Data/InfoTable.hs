module Juvix.Compiler.Concrete.Data.InfoTable where

import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

data OldFunctionInfo = OldFunctionInfo
  { _functionInfoType :: TypeSignature 'Scoped,
    _functionInfoClauses :: [FunctionClause 'Scoped]
  }
  deriving stock (Eq, Show)

data FunctionInfo
  = FunctionInfoOld OldFunctionInfo
  | FunctionInfoNew (NewTypeSignature 'Scoped)
  deriving stock (Eq, Show)

_FunctionInfoOld :: Traversal' FunctionInfo OldFunctionInfo
_FunctionInfoOld f = \case
  FunctionInfoOld x -> FunctionInfoOld <$> f x
  r@FunctionInfoNew {} -> pure r

data ConstructorInfo = ConstructorInfo
  { _constructorInfoDef :: InductiveConstructorDef 'Scoped,
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
    _infoFunctions :: HashMap S.NameId FunctionInfo
  }

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _infoConstructors = mempty,
      _infoAxioms = mempty,
      _infoModules = mempty,
      _infoInductives = mempty,
      _infoFunctions = mempty
    }

makeLenses ''InfoTable
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''AxiomInfo
makeLenses ''OldFunctionInfo

-- instance HasLoc FunctionInfo where
--   getLoc f =
--     getLoc (f ^. functionInfoType)
--       <>? (getLocSpan <$> nonEmpty (f ^. functionInfoClauses))
