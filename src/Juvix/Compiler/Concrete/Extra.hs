module Juvix.Compiler.Concrete.Extra
  ( module Juvix.Prelude.Parsing,
    getModuleFilePath,
    unfoldApplication,
    groupStatements,
    flattenStatement,
    recordNameSignatureByIndex,
    getExpressionAtomIden,
    getPatternAtomIden,
    isBodyExpression,
    isFunctionLike,
    isLhsFunctionLike,
    isFunctionRecursive,
    isLhsFunctionRecursive,
    symbolParsed,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude hiding (some)
import Juvix.Prelude.Parsing

getModuleFilePath :: Module s r -> Path Abs File
getModuleFilePath m = getLoc (m ^. moduleKw) ^. intervalFile

unfoldApplication :: Application -> (Expression, [Expression])
unfoldApplication (Application l r) = go [r] l
  where
    go :: [Expression] -> Expression -> (Expression, [Expression])
    go ac = \case
      ExpressionApplication (Application l' r') -> go (r' : ac) l'
      e -> (e, ac)

groupStatements :: forall s. (SingI s) => [Statement s] -> [NonEmpty (Statement s)]
groupStatements = \case
  [] -> []
  s : ss -> reverse . map NonEmpty.reverse . uncurry cons . foldl' aux (pure s, []) $ ss
  where
    aux ::
      (NonEmpty (Statement s), [NonEmpty (Statement s)]) ->
      Statement s ->
      (NonEmpty (Statement s), [NonEmpty (Statement s)])
    aux (gr@(a :| _), acc) b
      | g a b = (NonEmpty.cons b gr, acc)
      | otherwise = (pure b, gr : acc)
    -- Decides if statements a and b should be next to each other without a
    -- blank line
    g :: Statement s -> Statement s -> Bool
    g a b = case (a, b) of
      (StatementReservedInductive _, _) -> False
      (_, StatementReservedInductive _) -> False
      (StatementDeriving _, _) -> False
      (StatementSyntax _, StatementSyntax _) -> True
      (StatementSyntax (SyntaxFixity _), _) -> False
      (StatementSyntax (SyntaxOperator o), s) -> definesIdentifier (o ^. opSymbol) s
      (StatementSyntax (SyntaxIterator i), s) -> definesIdentifier (i ^. iterSymbol) s
      (StatementSyntax (SyntaxAlias {}), _) -> False
      (StatementImport _, StatementImport _) -> True
      (StatementImport i, StatementOpenModule o) -> case sing :: SStage s of
        SParsed -> True
        SScoped -> i ^. importModulePath . S.nameId == o ^. openModuleName . S.nameId
      (StatementImport _, _) -> False
      (StatementOpenModule {}, StatementOpenModule {}) -> True
      (StatementOpenModule {}, _) -> False
      (StatementInductive {}, _) -> False
      (StatementModule {}, _) -> False
      (StatementAxiom {}, StatementAxiom {}) -> False
      (StatementAxiom {}, _) -> False
      (StatementFunctionDef {}, _) -> False
      (_, StatementFunctionDef {}) -> False
      (StatementProjectionDef {}, StatementProjectionDef {}) -> True
      (StatementProjectionDef {}, _) -> False

    definesIdentifier :: IdentifierType s -> Statement s -> Bool
    definesIdentifier i stm = maybe False (`definesSymbol` stm) $ case sing :: SStage s of
      SParsed -> i ^? _NameUnqualified
      SScoped -> i ^? scopedIdenSrcName . S.nameConcrete . _NameUnqualified

    definesSymbol :: Symbol -> Statement s -> Bool
    definesSymbol n s = case s of
      StatementInductive d -> n `elem` syms d
      StatementAxiom d -> n == symbolParsed (d ^. axiomName)
      StatementFunctionDef d -> withFunctionSymbol False (\n' -> n == symbolParsed n') (d ^. functionDefName)
      _ -> False
      where
        syms :: InductiveDef s -> [Symbol]
        syms InductiveDef {..} =
          let constructors = toList _inductiveConstructors
           in case sing :: SStage s of
                SParsed -> _inductiveName : map (^. constructorName) constructors
                SScoped ->
                  _inductiveName
                    ^. S.nameConcrete
                    : map (^. constructorName . S.nameConcrete) constructors

flattenStatement :: Statement s -> [Statement s]
flattenStatement = \case
  StatementModule m -> concatMap flattenStatement (m ^. moduleBody)
  s -> [s]

recordNameSignatureByIndex :: RecordNameSignature s -> IntMap (NameItem s)
recordNameSignatureByIndex = indexedByInt (^. nameItemIndex) . (^. recordNames)

getExpressionAtomIden :: ExpressionAtom 'Scoped -> Maybe S.Name
getExpressionAtomIden = \case
  AtomIdentifier nm -> Just (nm ^. scopedIdenSrcName)
  _ -> Nothing

getPatternAtomIden :: PatternAtom 'Scoped -> Maybe S.Name
getPatternAtomIden = \case
  PatternAtomIden i -> case i of
    PatternScopedConstructor c -> Just (c ^. scopedIdenSrcName)
    _ -> Nothing
  _ -> Nothing

isBodyExpression :: FunctionDefBody a -> Bool
isBodyExpression = \case
  SigBodyExpression {} -> True
  SigBodyClauses {} -> False

isLhsFunctionLike :: FunctionLhs 'Parsed -> Bool
isLhsFunctionLike FunctionLhs {..} = notNull (_funLhsTypeSig ^. typeSigArgs)

isFunctionLike :: FunctionDef 'Parsed -> Bool
isFunctionLike d@FunctionDef {..} =
  isLhsFunctionLike (d ^. functionDefLhs) || (not . isBodyExpression) _functionDefBody

isFunctionRecursive :: FunctionDef 'Parsed -> Bool
isFunctionRecursive d = case d ^. functionDefLhs . funLhsIsTop of
  FunctionTop -> isLhsFunctionRecursive (d ^. functionDefLhs)
  FunctionNotTop -> isFunctionLike d

isLhsFunctionRecursive :: FunctionLhs 'Parsed -> Bool
isLhsFunctionRecursive d = case d ^. funLhsIsTop of
  FunctionTop -> case d ^. funLhsName of
    FunctionDefNamePattern {} -> False
    FunctionDefName {} -> True
  FunctionNotTop -> isLhsFunctionLike d
