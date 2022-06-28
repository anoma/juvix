module MiniJuvix.Syntax.MonoJuvix.InfoTable where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MonoJuvix.Language

data ConstructorInfo = ConstructorInfo
  { _constructorInfoArgs :: [Type],
    _constructorInfoInductive :: InductiveName,
    _constructorInfoBuiltin :: Maybe BuiltinConstructor
  }

newtype InductiveInfo = InductiveInfo
  { _inductiveInfoBuiltin :: Maybe BuiltinInductive
  }

data FunctionInfo = FunctionInfo
  { _functionInfoType :: Type,
    _functionInfoPatterns :: Int,
    _functionInfoBuiltin :: Maybe BuiltinFunction
  }

data AxiomInfo = AxiomInfo
  { _axiomInfoType :: Type,
    _axiomInfoBuiltin :: Maybe BuiltinAxiom
  }

data InfoTable = InfoTable
  { _infoInductives :: HashMap Name InductiveInfo,
    _infoConstructors :: HashMap Name ConstructorInfo,
    _infoAxioms :: HashMap Name AxiomInfo,
    _infoFunctions :: HashMap Name FunctionInfo
  }

buildTable :: Module -> InfoTable
buildTable m = InfoTable {..}
  where
    _infoInductives :: HashMap Name InductiveInfo
    _infoInductives =
      HashMap.fromList
        [ (d ^. inductiveName, InductiveInfo (d ^. inductiveBuiltin))
          | StatementInductive d <- ss
        ]
    _infoConstructors :: HashMap Name ConstructorInfo
    _infoConstructors =
      HashMap.fromList
        [ ( c ^. constructorName,
            ConstructorInfo
              { _constructorInfoArgs = args,
                _constructorInfoInductive = ind,
                _constructorInfoBuiltin = builtin
              }
          )
          | StatementInductive d <- ss,
            let ind = d ^. inductiveName,
            let n = length (d ^. inductiveConstructors),
            let builtins = maybe (replicate n Nothing) (map Just . builtinConstructors) (d ^. inductiveBuiltin),
            (builtin, c) <- zipExact builtins (d ^. inductiveConstructors),
            let args = c ^. constructorParameters
        ]
    _infoFunctions :: HashMap Name FunctionInfo
    _infoFunctions =
      HashMap.fromList
        [ ( f ^. funDefName,
            FunctionInfo
              { _functionInfoType = f ^. funDefType,
                _functionInfoPatterns = length (head (f ^. funDefClauses) ^. clausePatterns),
                _functionInfoBuiltin = f ^. funDefBuiltin
              }
          )
          | StatementFunction f <- ss
        ]
    _infoAxioms :: HashMap Name AxiomInfo
    _infoAxioms =
      HashMap.fromList
        [ ( d ^. axiomName,
            AxiomInfo
              { _axiomInfoType = d ^. axiomType,
                _axiomInfoBuiltin = d ^. axiomBuiltin
              }
          )
          | StatementAxiom d <- ss
        ]
    ss = m ^. (moduleBody . moduleStatements)

makeLenses ''InfoTable
makeLenses ''FunctionInfo
makeLenses ''ConstructorInfo
makeLenses ''InductiveInfo
makeLenses ''AxiomInfo

lookupConstructor :: Member (Reader InfoTable) r => Name -> Sem r ConstructorInfo
lookupConstructor c = asks (^?! infoConstructors . at c . _Just)

lookupInductive :: Member (Reader InfoTable) r => Name -> Sem r InductiveInfo
lookupInductive c = asks (^?! infoInductives . at c . _Just)

lookupFunction :: Member (Reader InfoTable) r => Name -> Sem r FunctionInfo
lookupFunction c = asks (^?! infoFunctions . at c . _Just)

lookupAxiom :: Member (Reader InfoTable) r => Name -> Sem r AxiomInfo
lookupAxiom c = asks (^?! infoAxioms . at c . _Just)
