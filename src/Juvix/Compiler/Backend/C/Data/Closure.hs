module Juvix.Compiler.Backend.C.Data.Closure where

import Juvix.Compiler.Backend.C.Data.Base
import Juvix.Compiler.Backend.C.Language
import Juvix.Compiler.Concrete.Data.Builtins (IsBuiltin (toBuiltinPrim))
import Juvix.Compiler.Internal.Extra (mkPolyType')
import Juvix.Compiler.Internal.Extra qualified as Micro
import Juvix.Compiler.Internal.Translation.Extra qualified as Micro
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Micro
import Juvix.Prelude

genClosures ::
  forall r.
  Members '[Reader Micro.InfoTable, Reader Micro.TypesTable] r =>
  Micro.Module ->
  Sem r [CCode]
genClosures Micro.Module {..} = do
  closureInfos <- concatMapM (applyOnFunStatement functionDefClosures) (_moduleBody ^. Micro.moduleStatements)
  return (genCClosure =<< nub closureInfos)

genCClosure :: ClosureInfo -> [CCode]
genCClosure c =
  [ ExternalDecl (genClosureEnv c),
    ExternalFunc (genClosureApply c),
    ExternalFunc (genClosureEval c)
  ]

functionDefClosures ::
  Members '[Reader Micro.InfoTable, Reader Micro.TypesTable] r =>
  Micro.FunctionDef ->
  Sem r [ClosureInfo]
functionDefClosures Micro.FunctionDef {..} =
  concatMapM (clauseClosures (fst (unfoldFunType (mkPolyType' _funDefType)))) (toList _funDefClauses)

lookupBuiltinIden :: Members '[Reader Micro.InfoTable] r => Micro.Iden -> Sem r (Maybe Micro.BuiltinPrim)
lookupBuiltinIden = \case
  Micro.IdenFunction f -> fmap toBuiltinPrim . (^. Micro.functionInfoDef . Micro.funDefBuiltin) <$> Micro.lookupFunction f
  Micro.IdenConstructor c -> fmap toBuiltinPrim . (^. Micro.constructorInfoBuiltin) <$> Micro.lookupConstructor c
  Micro.IdenAxiom a -> fmap toBuiltinPrim . (^. Micro.axiomInfoBuiltin) <$> Micro.lookupAxiom a
  Micro.IdenVar {} -> return Nothing
  Micro.IdenInductive {} -> impossible

genClosureExpression ::
  forall r.
  Members '[Reader Micro.InfoTable, Reader Micro.TypesTable, Reader PatternInfoTable] r =>
  [Micro.PolyType] ->
  Micro.Expression ->
  Sem r [ClosureInfo]
genClosureExpression funArgTyps = \case
  Micro.ExpressionIden i -> do
    let rootFunMicroName = Micro.getName i
        rootFunNameId = rootFunMicroName ^. Micro.nameId
        rootFunName = mkName rootFunMicroName
    builtin <- lookupBuiltinIden i
    case i of
      Micro.IdenVar {} -> return []
      _ -> do
        (t, patterns) <- getType i
        let argTyps = t ^. cFunArgTypes
        if
            | null argTyps -> return []
            | otherwise ->
                return
                  [ ClosureInfo
                      { _closureNameId = rootFunNameId,
                        _closureRootName = rootFunName,
                        _closureBuiltin = builtin,
                        _closureMembers = [],
                        _closureFunType = t,
                        _closureCArity = patterns
                      }
                  ]
  Micro.ExpressionApplication a -> exprApplication a
  Micro.ExpressionLiteral {} -> return []
  Micro.ExpressionFunction {} -> impossible
  Micro.ExpressionHole {} -> impossible
  Micro.ExpressionUniverse {} -> impossible
  Micro.ExpressionSimpleLambda {} -> impossible
  Micro.ExpressionLambda {} -> impossible
  where
    exprApplication :: Micro.Application -> Sem r [ClosureInfo]
    exprApplication a = do
      (f0, appArgs) <- Micro.unfoldPolyApplication a
      if
          | null appArgs -> genClosureExpression funArgTyps f0
          | otherwise -> case f0 of
              Micro.ExpressionLiteral {} -> return []
              Micro.ExpressionIden f -> do
                let rootFunMicroName = Micro.getName f
                    rootFunNameId = rootFunMicroName ^. Micro.nameId
                    rootFunName = mkName rootFunMicroName
                builtin <- lookupBuiltinIden f
                (fType, patterns) <- getType f
                closureArgs <- concatMapM (genClosureExpression funArgTyps) (toList appArgs)
                if
                    | length appArgs < length (fType ^. cFunArgTypes) ->
                        return
                          ( [ ClosureInfo
                                { _closureNameId = rootFunNameId,
                                  _closureRootName = rootFunName,
                                  _closureBuiltin = builtin,
                                  _closureMembers = take (length appArgs) (fType ^. cFunArgTypes),
                                  _closureFunType = fType,
                                  _closureCArity = patterns
                                }
                            ]
                              <> closureArgs
                          )
                    | otherwise -> return closureArgs
              _ -> impossible

genClosureEnv :: ClosureInfo -> Declaration
genClosureEnv c =
  typeDefWrap
    (asTypeDef name)
    ( DeclStructUnion
        ( StructUnion
            { _structUnionTag = StructTag,
              _structUnionName = Just name,
              _structMembers = Just (funDecl : members)
            }
        )
    )
  where
    name :: Text
    name = asEnv (closureNamedId c)
    funDecl :: Declaration
    funDecl = namedDeclType funField uIntPtrType
    members :: [Declaration]
    members = uncurry cDeclToNamedDecl <$> zip envArgs (c ^. closureMembers)

genClosureApplySig :: ClosureInfo -> FunctionSig
genClosureApplySig c = cFunTypeToFunSig (asApply (closureNamedId c)) applyFunType
  where
    nonEnvTyps :: [CDeclType]
    nonEnvTyps = drop (length (c ^. closureMembers)) (c ^. closureFunType . cFunArgTypes)
    allFunTyps :: [CDeclType]
    allFunTyps = declFunctionPtrType : nonEnvTyps
    applyFunType :: CFunType
    applyFunType = (c ^. closureFunType) {_cFunArgTypes = allFunTyps}

genClosureApply :: ClosureInfo -> Function
genClosureApply c =
  let localName :: Text
      localName = "env"
      localFunName :: Text
      localFunName = "f"
      name :: Text
      name = closureNamedId c
      envName :: Text
      envName = asTypeDef (asEnv name)
      closureEnvArgs :: [Text]
      closureEnvArgs = take (length (c ^. closureMembers)) envArgs
      closureEnvAccess :: [Expression]
      closureEnvAccess = memberAccess Pointer (ExpressionVar localName) <$> closureEnvArgs
      args :: [Expression]
      args = take (length (c ^. closureFunType . cFunArgTypes)) (closureEnvAccess <> drop 1 (ExpressionVar <$> funArgs))
      nPatterns :: Int
      nPatterns = c ^. closureCArity
      patternArgs :: [Expression]
      patternArgs = take nPatterns args
      funType :: CFunType
      funType =
        (c ^. closureFunType)
          { _cFunArgTypes = drop nPatterns (c ^. closureFunType . cFunArgTypes)
          }
      localFunType :: CFunType
      localFunType =
        (c ^. closureFunType)
          { _cFunArgTypes = take nPatterns (c ^. closureFunType . cFunArgTypes)
          }
      funName :: Expression
      funName = ExpressionVar (c ^. closureRootName)
      funCall :: Expression
      funCall =
        if
            | null patternArgs -> funName
            | otherwise -> functionCallCasted localFunType funName patternArgs
      juvixFunCall :: [BodyItem]
      juvixFunCall =
        if
            | nPatterns < length args ->
                [ BodyDecl
                    ( Declaration
                        { _declType = declFunctionType,
                          _declIsPtr = True,
                          _declName = Just localFunName,
                          _declInitializer = Just (ExprInitializer funCall)
                        }
                    ),
                  BodyStatement . StatementReturn . Just $ juvixFunctionCall funType (ExpressionVar localFunName) (drop nPatterns args)
                ]
            | otherwise ->
                [ BodyStatement . StatementReturn . Just $
                    functionCallCasted (c ^. closureFunType) (ExpressionVar (closureRootFunction c)) args
                ]
      envArg :: BodyItem
      envArg =
        BodyDecl
          ( Declaration
              { _declType = DeclTypeDefType envName,
                _declIsPtr = True,
                _declName = Just localName,
                _declInitializer =
                  Just $
                    ExprInitializer
                      ( castToType
                          ( CDeclType
                              { _typeDeclType = DeclTypeDefType envName,
                                _typeIsPtr = True
                              }
                          )
                          (ExpressionVar "fa0")
                      )
              }
          )
   in Function
        { _funcSig = genClosureApplySig c,
          _funcBody = envArg : juvixFunCall
        }

genClosureEval :: ClosureInfo -> Function
genClosureEval c =
  let localName :: Text
      localName = "f"
      name :: Text
      name = closureNamedId c
      envName :: Text
      envName = asTypeDef (asEnv name)
      envArgToFunArg :: [(Text, Text)]
      envArgToFunArg = take (length (c ^. closureMembers)) (zip envArgs funArgs)
      assignments :: [Assign]
      assignments = mkAssign <$> envArgToFunArg
      mkAssign :: (Text, Text) -> Assign
      mkAssign (envArg, funArg) =
        Assign
          { _assignLeft = memberAccess Pointer (ExpressionVar localName) envArg,
            _assignRight = ExpressionVar funArg
          }
   in Function
        { _funcSig =
            FunctionSig
              { _funcReturnType = declFunctionType,
                _funcIsPtr = True,
                _funcQualifier = None,
                _funcName = asEval name,
                _funcArgs = namedArgs asFunArg (c ^. closureMembers)
              },
          _funcBody =
            [ BodyDecl
                ( Declaration
                    { _declType = DeclTypeDefType envName,
                      _declIsPtr = True,
                      _declName = Just localName,
                      _declInitializer = Just $ ExprInitializer (mallocSizeOf envName)
                    }
                ),
              BodyStatement
                ( StatementExpr
                    ( ExpressionAssign
                        ( Assign
                            { _assignLeft = memberAccess Pointer (ExpressionVar localName) funField,
                              _assignRight =
                                castToType
                                  ( CDeclType
                                      { _typeDeclType = uIntPtrType,
                                        _typeIsPtr = False
                                      }
                                  )
                                  (ExpressionVar (asApply name))
                            }
                        )
                    )
                )
            ]
              <> (BodyStatement . StatementExpr . ExpressionAssign <$> assignments)
              <> [ returnStatement (castToType declFunctionPtrType (ExpressionVar localName))
                 ]
        }

clauseClosures ::
  Members '[Reader Micro.InfoTable, Reader Micro.TypesTable] r =>
  [Micro.PolyType] ->
  Micro.FunctionClause ->
  Sem r [ClosureInfo]
clauseClosures argTyps clause = do
  bindings <- buildPatternInfoTable argTyps clause
  runReader bindings (genClosureExpression argTyps (clause ^. Micro.clauseBody))
