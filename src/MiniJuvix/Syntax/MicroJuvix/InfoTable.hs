module MiniJuvix.Syntax.MicroJuvix.InfoTable where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language
import MiniJuvix.Syntax.Backends
import qualified Data.HashMap.Strict as HashMap

data ConstructorInfo = ConstructorInfo {
  _constructorInfoArgs :: [Type],
  _constructorInfoInductive :: InductiveName
  }

newtype FunctionInfo = FunctionInfo {
  _functionInfoType :: Type
  }

data AxiomInfo = AxiomInfo {
  _axiomInfoType :: Type,
  _axiomInfoBackends :: [BackendItem]
  }

data InfoTable = InfoTable {
  _infoConstructors :: HashMap Name ConstructorInfo,
  _infoAxioms :: HashMap Name AxiomInfo,
  _infoFunctions :: HashMap Name FunctionInfo
  }

buildTable :: Module -> InfoTable
buildTable m = InfoTable {..}
  where
   _infoConstructors :: HashMap Name ConstructorInfo
   _infoConstructors = HashMap.fromList
     [ (c ^. constructorName, ConstructorInfo args ind) |
       StatementInductive d <- ss,
       let ind = d ^. inductiveName,
       c <- d ^. inductiveConstructors,
       let args = c ^. constructorParameters
     ]
   _infoFunctions :: HashMap Name FunctionInfo
   _infoFunctions = HashMap.fromList
     [ (f ^. funDefName, FunctionInfo (f ^. funDefType)) |
       StatementFunction f <- ss]
   _infoAxioms :: HashMap Name AxiomInfo
   _infoAxioms = HashMap.fromList
     [ (d ^. axiomName , AxiomInfo (d ^. axiomType) (d ^. axiomBackendItems))
       | StatementAxiom d <- ss ]
   ss = m ^. (moduleBody . moduleStatements)

makeLenses ''InfoTable
makeLenses ''FunctionInfo
makeLenses ''ConstructorInfo
makeLenses ''AxiomInfo
