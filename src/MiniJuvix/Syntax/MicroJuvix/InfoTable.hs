module MiniJuvix.Syntax.MicroJuvix.InfoTable where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language

data ConstructorInfo = ConstructorInfo
  { _constructorInfoInductiveParameters :: [InductiveParameter],
    _constructorInfoArgs :: [Type],
    _constructorInfoInductive :: InductiveName
  }

newtype FunctionInfo = FunctionInfo
  { _functionInfoDef :: FunctionDef
  }

newtype AxiomInfo = AxiomInfo
  { _axiomInfoType :: Type
  }

newtype InductiveInfo = InductiveInfo
  { _inductiveInfoDef :: InductiveDef
  }

data InfoTable = InfoTable
  { _infoConstructors :: HashMap Name ConstructorInfo,
    _infoAxioms :: HashMap Name AxiomInfo,
    _infoFunctions :: HashMap Name FunctionInfo,
    _infoInductives :: HashMap Name InductiveInfo
  }

makeLenses ''InfoTable
makeLenses ''FunctionInfo
makeLenses ''ConstructorInfo
makeLenses ''AxiomInfo
makeLenses ''InductiveInfo

instance Semigroup InfoTable where
  a <> b =
    InfoTable
      { _infoConstructors = a ^. infoConstructors <> b ^. infoConstructors,
        _infoAxioms = a ^. infoAxioms <> b ^. infoAxioms,
        _infoFunctions = a ^. infoFunctions <> b ^. infoFunctions,
        _infoInductives = a ^. infoInductives <> b ^. infoInductives
      }

instance Monoid InfoTable where
  mempty =
    InfoTable
      { _infoConstructors = mempty,
        _infoAxioms = mempty,
        _infoFunctions = mempty,
        _infoInductives = mempty
      }

buildTable :: Foldable f => f Module -> InfoTable
buildTable = mconcatMap buildTable1

buildTable1 :: Module -> InfoTable
buildTable1 m = InfoTable {..}
  where
    _infoInductives :: HashMap Name InductiveInfo
    _infoInductives =
      HashMap.fromList
        [ (d ^. inductiveName, InductiveInfo d)
          | StatementInductive d <- ss
        ]
    _infoConstructors :: HashMap Name ConstructorInfo
    _infoConstructors =
      HashMap.fromList
        [ (c ^. constructorName, ConstructorInfo params args ind)
          | StatementInductive d <- ss,
            let ind = d ^. inductiveName,
            let params = d ^. inductiveParameters,
            c <- d ^. inductiveConstructors,
            let args = c ^. constructorParameters
        ]
    _infoFunctions :: HashMap Name FunctionInfo
    _infoFunctions =
      HashMap.fromList
        [ (f ^. funDefName, FunctionInfo f)
          | StatementFunction f <- ss
        ]
    _infoAxioms :: HashMap Name AxiomInfo
    _infoAxioms =
      HashMap.fromList
        [ (d ^. axiomName, AxiomInfo (d ^. axiomType))
          | StatementAxiom d <- ss
        ]
    ss = m ^. (moduleBody . moduleStatements)

lookupConstructor :: Member (Reader InfoTable) r => Name -> Sem r ConstructorInfo
lookupConstructor f = HashMap.lookupDefault impossible f <$> asks _infoConstructors

lookupInductive :: Member (Reader InfoTable) r => InductiveName -> Sem r InductiveInfo
lookupInductive f = HashMap.lookupDefault impossible f <$> asks _infoInductives

lookupFunction :: Member (Reader InfoTable) r => Name -> Sem r FunctionInfo
lookupFunction f = HashMap.lookupDefault impossible f <$> asks _infoFunctions

lookupAxiom :: Member (Reader InfoTable) r => Name -> Sem r AxiomInfo
lookupAxiom f = HashMap.lookupDefault impossible f <$> asks _infoAxioms
