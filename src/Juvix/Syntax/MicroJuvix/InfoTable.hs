module Juvix.Syntax.MicroJuvix.InfoTable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.Language.Extra

data ConstructorInfo = ConstructorInfo
  { _constructorInfoInductiveParameters :: [InductiveParameter],
    _constructorInfoArgs :: [Expression],
    _constructorInfoInductive :: InductiveName,
    _constructorInfoBuiltin :: Maybe BuiltinConstructor
  }

newtype FunctionInfo = FunctionInfo
  { _functionInfoDef :: FunctionDef
  }

data AxiomInfo = AxiomInfo
  { _axiomInfoType :: Expression,
    _axiomInfoBuiltin :: Maybe BuiltinAxiom
  }

newtype InductiveInfo = InductiveInfo
  { _inductiveInfoDef :: InductiveDef
  }

data InfoTable = InfoTable
  { _infoConstructors :: HashMap Name ConstructorInfo,
    _infoAxioms :: HashMap Name AxiomInfo,
    _infoFunctions :: HashMap Name FunctionInfo,
    _infoInductives :: HashMap Name InductiveInfo,
    _infoEval :: HashMap FunctionName Expression
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
        _infoInductives = a ^. infoInductives <> b ^. infoInductives,
        _infoEval = a ^. infoEval <> b ^. infoEval
      }

instance Monoid InfoTable where
  mempty =
    InfoTable
      { _infoConstructors = mempty,
        _infoAxioms = mempty,
        _infoFunctions = mempty,
        _infoInductives = mempty,
        _infoEval = mempty
      }

buildTable :: Foldable f => f Module -> InfoTable
buildTable = mconcatMap buildTable1

-- TODO avoid building a table for the same module twice
buildTable1 :: Module -> InfoTable
buildTable1 m = InfoTable {..} <> buildTable (map (^. includeModule) includes)
  where
    includes :: [Include]
    includes = [i | StatementInclude i <- ss]
    _infoInductives :: HashMap Name InductiveInfo
    _infoInductives =
      HashMap.fromList
        [ (d ^. inductiveName, InductiveInfo d)
          | StatementInductive d <- ss
        ]
    _infoConstructors :: HashMap Name ConstructorInfo
    _infoConstructors =
      HashMap.fromList
        [ (c ^. inductiveConstructorName, ConstructorInfo params args ind builtin)
          | StatementInductive d <- ss,
            let ind = d ^. inductiveName,
            let n = length (d ^. inductiveConstructors),
            let params = d ^. inductiveParameters,
            let builtins = maybe (replicate n Nothing) (map Just . builtinConstructors) (d ^. inductiveBuiltin),
            (builtin, c) <- zipExact builtins (d ^. inductiveConstructors),
            let args = c ^. inductiveConstructorParameters
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
        [ (d ^. axiomName, AxiomInfo (d ^. axiomType) (d ^. axiomBuiltin))
          | StatementAxiom d <- ss
        ]

    _infoEval :: HashMap FunctionName Expression
    _infoEval = HashMap.fromList [(fun ^. funDefName, e) |
               StatementFunction fun <- ss, Just e <- [functionDefEval fun]
            ]

    ss :: [Statement]
    ss = m ^. (moduleBody . moduleStatements)

functionDefEval :: FunctionDef -> Maybe Expression
functionDefEval f = case f ^. funDefClauses of
  c :| [] -> goClause c
  _ -> Nothing
  where
  goClause :: FunctionClause -> Maybe Expression
  goClause c = go (c ^. clausePatterns)
    where
    go :: [PatternArg] -> Maybe Expression
    go = undefined

lookupConstructor :: Member (Reader InfoTable) r => Name -> Sem r ConstructorInfo
lookupConstructor f = HashMap.lookupDefault impossible f <$> asks (^. infoConstructors)

lookupInductive :: Member (Reader InfoTable) r => InductiveName -> Sem r InductiveInfo
lookupInductive f = HashMap.lookupDefault impossible f <$> asks (^. infoInductives)

lookupFunction :: Member (Reader InfoTable) r => Name -> Sem r FunctionInfo
lookupFunction f = HashMap.lookupDefault impossible f <$> asks (^. infoFunctions)

lookupAxiom :: Member (Reader InfoTable) r => Name -> Sem r AxiomInfo
lookupAxiom f = HashMap.lookupDefault impossible f <$> asks (^. infoAxioms)

inductiveType :: Member (Reader InfoTable) r => Name -> Sem r Expression
inductiveType v = do
  info <- lookupInductive v
  let ps = info ^. inductiveInfoDef . inductiveParameters
  return $
    foldr
      (\p k -> ExpressionFunction (typeAbs (p ^. inductiveParamName) k))
      (ExpressionUniverse (SmallUniverse (getLoc v)))
      ps
  where
    typeAbs var = Function (typeAbstraction Explicit var)

constructorArgTypes :: ConstructorInfo -> ([VarName], [Expression])
constructorArgTypes i =
  ( map (^. inductiveParamName) (i ^. constructorInfoInductiveParameters),
    i ^. constructorInfoArgs
  )

constructorType :: Member (Reader InfoTable) r => ConstrName -> Sem r Expression
constructorType c = do
  info <- lookupConstructor c
  let (inductiveParams, constrArgs) = constructorArgTypes info
      args =
        map (typeAbstraction Implicit) inductiveParams
          ++ map unnamedParameter constrArgs
      ind = ExpressionIden (IdenInductive (info ^. constructorInfoInductive))
      saturatedTy =
        foldl'
          ( \t v ->
              ExpressionApplication
                ( Application
                    { _appLeft = t,
                      _appRight = ExpressionIden (IdenVar v),
                      _appImplicit = Explicit
                    }
                )
          )
          ind
          inductiveParams
  return (foldFunType args saturatedTy)
