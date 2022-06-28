module MiniJuvix.Translation.MonoJuvixToMiniC.Closure where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MiniC.Language
import MiniJuvix.Syntax.MonoJuvix.Language qualified as Mono
import MiniJuvix.Translation.MicroJuvixToMonoJuvix qualified as Mono
import MiniJuvix.Translation.MonoJuvixToMiniC.Base

genClosures ::
  forall r.
  Member (Reader Mono.InfoTable) r =>
  Mono.Module ->
  Sem r [CCode]
genClosures Mono.Module {..} = do
  closureInfos <- concatMapM (applyOnFunStatement functionDefClosures) (_moduleBody ^. Mono.moduleStatements)
  return (genCClosure =<< nub closureInfos)

genCClosure :: ClosureInfo -> [CCode]
genCClosure c =
  [ ExternalDecl (genClosureEnv c),
    ExternalFunc (genClosureApply c),
    ExternalFunc (genClosureEval c)
  ]

functionDefClosures ::
  Member (Reader Mono.InfoTable) r =>
  Mono.FunctionDef ->
  Sem r [ClosureInfo]
functionDefClosures Mono.FunctionDef {..} =
  concatMapM (clauseClosures (fst (unfoldFunType _funDefType))) (toList _funDefClauses)

genClosureExpression ::
  forall r.
  Members '[Reader Mono.InfoTable, Reader PatternInfoTable] r =>
  [Mono.Type] ->
  Mono.Expression ->
  Sem r [ClosureInfo]
genClosureExpression funArgTyps = \case
  Mono.ExpressionIden i -> do
    let rootFunMonoName = Mono.getName i
        rootFunNameId = rootFunMonoName ^. Mono.nameId
        rootFunName = mkName rootFunMonoName
    case i of
      Mono.IdenVar {} -> return []
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
                        _closureMembers = [],
                        _closureFunType = t,
                        _closureCArity = patterns
                      }
                  ]
  Mono.ExpressionApplication a -> exprApplication a
  Mono.ExpressionLiteral {} -> return []
  where
    exprApplication :: Mono.Application -> Sem r [ClosureInfo]
    exprApplication a = do
      (f, appArgs) <- unfoldApp a
      let rootFunMonoName = Mono.getName f
          rootFunNameId = rootFunMonoName ^. Mono.nameId
          rootFunName = mkName rootFunMonoName
      (fType, patterns) <- getType f
      closureArgs <- concatMapM (genClosureExpression funArgTyps) appArgs

      if
          | length appArgs < length (fType ^. cFunArgTypes) ->
              return
                ( [ ClosureInfo
                      { _closureNameId = rootFunNameId,
                        _closureRootName = rootFunName,
                        _closureMembers = take (length appArgs) (fType ^. cFunArgTypes),
                        _closureFunType = fType,
                        _closureCArity = patterns
                      }
                  ]
                    <> closureArgs
                )
          | otherwise -> return closureArgs

    unfoldApp :: Mono.Application -> Sem r (Mono.Iden, [Mono.Expression])
    unfoldApp Mono.Application {..} = case _appLeft of
      Mono.ExpressionApplication x -> do
        uf <- unfoldApp x
        return (second (_appRight :) uf)
      Mono.ExpressionIden i -> do
        return (i, [_appRight])
      Mono.ExpressionLiteral {} -> impossible

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
      funName :: Expression
      funName = ExpressionVar (c ^. closureRootName)
      funCall :: Expression
      funCall =
        if
            | null patternArgs -> funName
            | otherwise -> functionCall funName patternArgs
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
            | otherwise -> [BodyStatement . StatementReturn . Just $ functionCall (ExpressionVar (c ^. closureRootName)) args]
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
  Members '[Reader Mono.InfoTable] r =>
  [Mono.Type] ->
  Mono.FunctionClause ->
  Sem r [ClosureInfo]
clauseClosures argTyps clause = do
  bindings <- buildPatternInfoTable argTyps clause
  runReader bindings (genClosureExpression argTyps (clause ^. Mono.clauseBody))
