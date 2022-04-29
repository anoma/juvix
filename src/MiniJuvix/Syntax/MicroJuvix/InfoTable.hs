module MiniJuvix.Syntax.MicroJuvix.InfoTable where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Backends
import MiniJuvix.Syntax.MicroJuvix.Language

data ConstructorInfo = ConstructorInfo
  { _constructorInfoInductiveParameters :: [InductiveParameter],
    _constructorInfoArgs :: [Type],
    _constructorInfoInductive :: InductiveName
  }

newtype FunctionInfo = FunctionInfo
  { _functionInfoType :: Type
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

-- TODO temporary function.
buildTable :: Module -> InfoTable
buildTable m = InfoTable {..}
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
        [ (f ^. funDefName, FunctionInfo (f ^. funDefType))
          | StatementFunction f <- ss
        ]
    _infoAxioms :: HashMap Name AxiomInfo
    _infoAxioms =
      HashMap.fromList
        [ (d ^. axiomName, AxiomInfo (d ^. axiomType))
          | StatementAxiom d <- ss
        ]
    ss = m ^. (moduleBody . moduleStatements)

makeLenses ''InfoTable
makeLenses ''FunctionInfo
makeLenses ''ConstructorInfo
makeLenses ''AxiomInfo
makeLenses ''InductiveInfo
