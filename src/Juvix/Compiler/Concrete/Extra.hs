module Juvix.Compiler.Concrete.Extra
  ( module Juvix.Prelude.Parsing,
    mkScopedModule,
    getAllModules,
    getModuleFilePath,
    unfoldApplication,
    groupStatements,
    flattenStatement,
    recordNameSignatureByIndex,
    getExpressionAtomIden,
    getPatternAtomIden,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Concrete.Data.NameSignature.Base
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude hiding (some)
import Juvix.Prelude.Parsing

data ScopedModule = forall t. MkScopedModule (SModuleIsTop t) (Module 'Scoped t)

mkScopedModule :: forall t. (SingI t) => Module 'Scoped t -> ScopedModule
mkScopedModule = MkScopedModule sing

getAllModules :: Module 'Scoped 'ModuleTop -> HashMap S.NameId (Module 'Scoped 'ModuleTop)
getAllModules m = HashMap.fromList (fst (run (runOutputList (getAllModules' m))))

getAllModules' ::
  forall r.
  (Member (Output (S.NameId, Module 'Scoped 'ModuleTop)) r) =>
  Module 'Scoped 'ModuleTop ->
  Sem r ()
getAllModules' m = recordModule m
  where
    recordModule :: Module 'Scoped 'ModuleTop -> Sem r ()
    recordModule n = do
      output (n ^. modulePath . S.nameId, n)
      processModule (mkScopedModule n)

    processModule :: ScopedModule -> Sem r ()
    processModule (MkScopedModule _ w) = forM_ (w ^. moduleBody) processStatement

    processStatement :: Statement 'Scoped -> Sem r ()
    processStatement = \case
      StatementImport i -> recordModule (i ^. importModule . moduleRefModule)
      StatementModule n -> processModule (mkScopedModule n)
      StatementOpenModule n -> forM_ (getModuleRefTopModule (n ^. openModuleName)) recordModule
      _ -> return ()

    getModuleRefTopModule :: ModuleRef' c -> Maybe (Module 'Scoped 'ModuleTop)
    getModuleRefTopModule (ModuleRef' (isTop :&: ModuleRef'' {..})) = case isTop of
      SModuleLocal -> Nothing
      SModuleTop -> Just _moduleRefModule

getModuleFilePath :: Module s 'ModuleTop -> Path Abs File
getModuleFilePath m = getLoc (m ^. moduleKw) ^. intervalFile

unfoldApplication :: Application -> (Expression, [Expression])
unfoldApplication (Application l r) = go [r] l
  where
    go :: [Expression] -> Expression -> (Expression, [Expression])
    go ac = \case
      ExpressionApplication (Application l' r') -> go (r' : ac) l'
      e -> (e, ac)

groupStatements :: forall s. SingI s => [Statement s] -> [NonEmpty (Statement s)]
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
      (StatementSyntax _, StatementSyntax _) -> True
      (StatementSyntax (SyntaxFixity _), _) -> False
      (StatementSyntax (SyntaxOperator o), s) -> definesSymbol (o ^. opSymbol) s
      (StatementSyntax (SyntaxIterator i), s) -> definesSymbol (i ^. iterSymbol) s
      (StatementSyntax (SyntaxAlias {}), _) -> False
      (StatementImport _, StatementImport _) -> True
      (StatementImport i, StatementOpenModule o) -> case sing :: SStage s of
        SParsed -> True
        SScoped ->
          i
            ^. importModule
              . moduleRefModule
              . modulePath
              . S.nameId
            == getModuleRefNameId (o ^. openModuleName)
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
    definesSymbol :: Symbol -> Statement s -> Bool
    definesSymbol n s = case s of
      StatementInductive d -> n `elem` syms d
      StatementAxiom d -> n == symbolParsed (d ^. axiomName)
      StatementFunctionDef d -> n == symbolParsed (d ^. signName)
      _ -> False
      where
        symbolParsed :: SymbolType s -> Symbol
        symbolParsed sym = case sing :: SStage s of
          SParsed -> sym
          SScoped -> sym ^. S.nameConcrete

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

recordNameSignatureByIndex :: RecordNameSignature -> IntMap Symbol
recordNameSignatureByIndex = IntMap.fromList . (^.. recordNames . each . to swap)

getExpressionAtomIden :: ExpressionAtom 'Scoped -> Maybe S.Name
getExpressionAtomIden = \case
  AtomIdentifier nm -> Just (nm ^. scopedIdenName)
  _ -> Nothing

getPatternAtomIden :: PatternAtom 'Scoped -> Maybe S.Name
getPatternAtomIden = \case
  PatternAtomIden i -> case i of
    PatternScopedConstructor c -> Just (c ^. scopedIdenName)
    _ -> Nothing
  _ -> Nothing
