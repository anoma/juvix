module Juvix.Compiler.Backend.Isabelle.Translation.FromTyped where

import Juvix.Compiler.Backend.Isabelle.Language
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Extra.Paths qualified as P

fromInternal :: Internal.Module -> Theory
fromInternal Internal.Module {..} =
  Theory
    { _theoryName = _moduleName,
      _theoryImports = map (^. Internal.importModuleName) (_moduleBody ^. Internal.moduleImports),
      _theoryStatements = concatMap goMutualBlock (_moduleBody ^. Internal.moduleStatements)
    }

goMutualBlock :: Internal.MutualBlock -> [Statement]
goMutualBlock Internal.MutualBlock {..} = map goMutualStatement (toList _mutualStatements)

goMutualStatement :: Internal.MutualStatement -> Statement
goMutualStatement = \case
  Internal.StatementInductive x -> goInductiveDef x
  Internal.StatementFunction x -> goFunctionDef x
  Internal.StatementAxiom x -> goAxiomDef x

goInductiveDef :: Internal.InductiveDef -> Statement
goInductiveDef Internal.InductiveDef {..}
  | length _inductiveConstructors == 1
      && head' _inductiveConstructors ^. Internal.inductiveConstructorIsRecord =
      let tyargs = fst $ Internal.unfoldFunType $ head' _inductiveConstructors ^. Internal.inductiveConstructorType
       in StmtRecord
            Record
              { _recordName = _inductiveName,
                _recordParams = params,
                _recordFields = map goRecordField tyargs
              }
  | otherwise =
      StmtDatatype
        Datatype
          { _datatypeName = _inductiveName,
            _datatypeParams = params,
            _datatypeConstructors = map goConstructorDef _inductiveConstructors
          }
  where
    params = map goInductiveParameter _inductiveParameters

goInductiveParameter :: Internal.InductiveParameter -> Var
goInductiveParameter Internal.InductiveParameter {..} = Var _inductiveParamName

goRecordField :: Internal.FunctionParameter -> RecordField
goRecordField Internal.FunctionParameter {..} =
  RecordField
    { _recordFieldName = fromMaybe defaultName _paramName,
      _recordFieldType = goType _paramType
    }
  where
    defaultName =
      Name
        { _nameText = "_",
          _nameId = defaultId,
          _nameKind = KNameLocal,
          _namePretty = "",
          _nameLoc = defaultLoc,
          _nameFixity = Nothing
        }
    defaultLoc = singletonInterval $ mkInitialLoc P.noFile
    defaultId =
      NameId
        { _nameIdUid = 0,
          _nameIdModuleId = ModuleId "" "" ""
        }

goConstructorDef :: Internal.ConstructorDef -> Constructor
goConstructorDef Internal.ConstructorDef {..} =
  Constructor
    { _constructorName = _inductiveConstructorName,
      _constructorArgTypes = tyargs
    }
  where
    tyargs = map (goType . (^. Internal.paramType)) (fst $ Internal.unfoldFunType _inductiveConstructorType)

goFunctionDef :: Internal.FunctionDef -> Statement
goFunctionDef Internal.FunctionDef {..}
  | null _funDefArgsInfo =
      StmtDefinition
        Definition
          { _definitionName = _funDefName,
            _definitionType = goType _funDefType
          }
  | otherwise =
      StmtFunction
        Function
          { _functionName = _funDefName,
            _functionType = goType _funDefType
          }

goAxiomDef :: Internal.AxiomDef -> Statement
goAxiomDef Internal.AxiomDef {..}
  | argsNum == 0 =
      StmtDefinition
        Definition
          { _definitionName = _axiomName,
            _definitionType = goType _axiomType
          }
  | otherwise =
      StmtFunction
        Function
          { _functionName = _axiomName,
            _functionType = goType _axiomType
          }
  where
    argsNum = length $ fst $ Internal.unfoldFunType _axiomType

goType :: Internal.Expression -> Type
goType ty = case ty of
  Internal.ExpressionIden x -> goTypeIden x
  Internal.ExpressionApplication x -> goTypeApp x
  Internal.ExpressionFunction x -> goTypeFun x
  Internal.ExpressionLiteral {} -> unsupportedType ty
  Internal.ExpressionHole {} -> unsupportedType ty
  Internal.ExpressionInstanceHole {} -> unsupportedType ty
  Internal.ExpressionLet {} -> unsupportedType ty
  Internal.ExpressionUniverse {} -> unsupportedType ty
  Internal.ExpressionSimpleLambda {} -> unsupportedType ty
  Internal.ExpressionLambda {} -> unsupportedType ty
  Internal.ExpressionCase {} -> unsupportedType ty
  where
    unsupportedType :: Internal.Expression -> a
    unsupportedType e = error ("unsupported type: " <> Internal.ppTrace e)

goTypeIden :: Internal.Iden -> Type
goTypeIden = \case
  Internal.IdenFunction name -> TyInd $ IndApp name []
  Internal.IdenConstructor name -> error ("unsupported type: constructor " <> Internal.ppTrace name)
  Internal.IdenVar name -> TyVar $ Var name
  Internal.IdenAxiom name -> TyInd $ IndApp name []
  Internal.IdenInductive name -> TyInd $ IndApp name []

goTypeApp :: Internal.Application -> Type
goTypeApp app = TyInd $ IndApp name params
  where
    (ind, args) = Internal.unfoldApplication app
    params = map goType (toList args)
    name = case ind of
      Internal.ExpressionIden (Internal.IdenFunction n) -> n
      Internal.ExpressionIden (Internal.IdenAxiom n) -> n
      Internal.ExpressionIden (Internal.IdenInductive n) -> n
      _ -> error ("unsupported type: " <> Internal.ppTrace app)

goTypeFun :: Internal.Function -> Type
goTypeFun Internal.Function {..} = TyFun $ FunType l r
  where
    l = goType (_functionLeft ^. Internal.paramType)
    r = goType _functionRight
