{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module MiniJuvix.Parsing.Scoper where

--------------------------------------------------------------------------------

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Lens.Micro.Platform
import MiniJuvix.Parsing.Base (MonadParsec)
import qualified MiniJuvix.Parsing.Base as P
import MiniJuvix.Parsing.Language
import MiniJuvix.Parsing.Parser (runModuleParserIO)
import MiniJuvix.Utils.Prelude hiding (Reader, State, ask, asks, get, gets, local, modify, put)
import Polysemy
import Polysemy.Error hiding (fromEither)
import Polysemy.Reader
import Polysemy.State
import System.FilePath

--------------------------------------------------------------------------------

-- | Relevant scope information of a module.
data ModuleScopeInfo = ModuleScopeInfo
  { -- | Operator definitions. They can refer to either constructors or functions.
    _syntaxOperators :: [OperatorSyntaxDef],
    -- | constructors introduced by inductive definitions (E.g. zero; suc).
    _syntaxConstructors :: HashSet DataConstructorName,
    -- | data types  introduced by inductive definitions (E.g. ℕ).
    _syntaxDataTypes :: HashSet DataTypeName,
    -- | function names in scope. Function names are introduced with function clauses.
    _syntaxFunctions :: HashSet FunctionName,
    -- | locally defined modules. Imported modules are not included.
    _syntaxModules :: HashMap Symbol ModuleScopeInfo
  }

newtype IdentifierInfo = IdentifierInfo
  { idenInfoOrigins :: HashSet ModulePath
  }

data ModuleCurrentScope = ModuleGlobalScope
  { _currentOperators :: HashMap Symbol Fixity,
    _currentConstructors :: HashMap DataConstructorName IdentifierInfo,
    _currentFunctions :: HashMap FunctionName IdentifierInfo,
    _currentImported :: HashMap ModulePath ModuleScopeInfo
  }

makeLenses ''ModuleCurrentScope

newtype ModulesCache = ModulesCache
  { cachedModules :: HashMap ModulePath (Module 'Scoped 'ModuleTop)
  }

newtype LocalScope = LocalScope
  { localScopeSymbols :: HashSet Symbol
  }

data ScopeError
  = ParseError Text
  | Err
  | ErrImportCycle ModulePath
  | ErrOpenNotInScope ModulePath
  deriving stock (Show)

data ScopeParameters = ScopeParameters
  { -- | Root of the project.
    _scopeRootPath :: FilePath,
    -- | Usually set to ".mjuvix".
    _scopeFileExtension :: String,
    -- | Used for import cycle detection.
    _scopeTopParents :: HashSet ModulePath
  }

makeLenses ''ScopeParameters

scopeCheck :: FilePath -> [Module 'Parsed t] -> Either ScopeError [Module 'Scoped t]
scopeCheck = undefined

checkImport ::
  Members '[Error ScopeError, State ModuleCurrentScope, Reader ScopeParameters, Embed IO, State ModulesCache] r =>
  Import ->
  Sem r (Module 'Scoped 'ModuleTop)
checkImport (Import p) = readParseModule p >>= checkTopModule

moduleScopeInfo :: Module 'Scoped t -> ModuleScopeInfo
moduleScopeInfo (Module _ stmts) = ModuleScopeInfo {..}
  where
    _syntaxModules :: HashMap Symbol ModuleScopeInfo
    _syntaxModules = HashMap.fromList (mapMaybe getModule stmts)
      where
        getModule :: Statement 'Scoped -> Maybe (Symbol, ModuleScopeInfo)
        getModule s = case s of
          StatementModule m -> Just (moduleModulePath m, moduleScopeInfo m)
          _ -> Nothing
    _syntaxFunctions :: HashSet FunctionName
    _syntaxFunctions = HashSet.fromList (mapMaybe getFun stmts)
      where
        getFun :: Statement 'Scoped -> Maybe FunctionName
        getFun s = case s of
          -- StatementDataType DataTypeDef {..} → HashSet.fromList (map constructorName dataTypeConstructors)
          _ -> undefined
    _syntaxConstructors :: HashSet DataConstructorName
    _syntaxConstructors = mconcat (map getConstrs stmts)
      where
        getConstrs :: Statement 'Scoped -> HashSet DataConstructorName
        getConstrs s = case s of
          StatementDataType DataTypeDef {..} -> HashSet.fromList (map constructorName dataTypeConstructors)
          _ -> mempty
    _syntaxDataTypes :: HashSet DataTypeName
    _syntaxDataTypes = HashSet.fromList (mapMaybe getDT stmts)
      where
        getDT :: Statement 'Scoped -> Maybe DataTypeName
        getDT s = case s of
          StatementDataType DataTypeDef {..} -> Just dataTypeName
          _ -> Nothing
    _syntaxOperators :: [OperatorSyntaxDef]
    _syntaxOperators = mapMaybe getDef stmts
      where
        getDef s = case s of
          StatementOperator op -> Just op
          _ -> Nothing

readParseModule ::
  Members '[Error ScopeError, Reader ScopeParameters, Embed IO] r =>
  ModulePath ->
  Sem r (Module 'Parsed 'ModuleTop)
readParseModule mp = do
  path <- modulePathToFilePath mp
  res <- embed (runModuleParserIO path)
  case res of
    Left err -> throw (ParseError err)
    Right r -> return r

modulePathToFilePath ::
  Members '[Reader ScopeParameters] r =>
  ModulePath ->
  Sem r FilePath
modulePathToFilePath mp = do
  root <- asks _scopeRootPath
  ext <- asks _scopeFileExtension
  let relDirPath = foldr ((</>) . toPath) mempty (modulePathDir mp)
      relFilePath = relDirPath </> toPath (modulePathName mp) <.> ext
  return $ root </> relFilePath
  where
    toPath :: Symbol -> FilePath
    toPath (Sym t) = Text.unpack t

addOperatorSyntaxDef ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  OperatorSyntaxDef ->
  Sem r ()
addOperatorSyntaxDef = undefined

checkTypeSignature ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  TypeSignature 'Parsed ->
  Sem r (TypeSignature 'Scoped)
checkTypeSignature = undefined

checkDataTypeDef ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  DataTypeDef 'Parsed ->
  Sem r (DataTypeDef 'Scoped)
checkDataTypeDef = undefined

checkTopModule ::
  forall r.
  Members '[Error ScopeError, State ModuleCurrentScope, Reader ScopeParameters, State ModulesCache, Embed IO] r =>
  Module 'Parsed 'ModuleTop ->
  Sem r (Module 'Scoped 'ModuleTop)
checkTopModule (Module path stmts) = do
  checkCycle
  cache <- gets @ModulesCache cachedModules
  maybe checkedModule return (cache ^. at path)
  where
    addParent :: ScopeParameters -> ScopeParameters
    addParent = over scopeTopParents (HashSet.insert path)
    checkedModule :: Sem r (Module 'Scoped 'ModuleTop)
    checkedModule =
      Module path
        <$> local addParent (mapM checkStatement stmts)
    checkCycle :: Sem r ()
    checkCycle =
      whenM
        (HashSet.member path <$> asks _scopeTopParents)
        (throw (ErrImportCycle path))

checkLocalModule ::
  forall r.
  Members '[Error ScopeError, State ModuleCurrentScope, Reader ScopeParameters, State ModulesCache, Embed IO] r =>
  Module 'Parsed 'ModuleLocal ->
  Sem r (Module 'Scoped 'ModuleLocal)
checkLocalModule (Module path stmts) = Module path <$> mapM checkStatement stmts

checkOpenModule ::
  forall r.
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  OpenModule ->
  Sem r ()
checkOpenModule OpenModule {..} = do
  info <- openInfo
  openModule openModuleName info openUsingHiding
  where
    openInfo :: Sem r ModuleScopeInfo
    openInfo = do
      r <- HashMap.lookup openModuleName <$> gets _currentImported
      case r of
        Just info -> return info
        _ -> throw (ErrOpenNotInScope openModuleName)

openOperatorSyntaxDef ::
  forall r.
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  ModulePath ->
  OperatorSyntaxDef ->
  Sem r ()
openOperatorSyntaxDef path d = undefined

openModule ::
  forall r.
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  ModulePath ->
  ModuleScopeInfo ->
  Maybe UsingHiding ->
  Sem r ()
openModule openPath ModuleScopeInfo {..} uh = do
  mapM_ (openOperatorSyntaxDef openPath) (filter (shouldOpen . opSymbol) _syntaxOperators)
  where
    setsUsingHiding :: Maybe (Either (HashSet Symbol) (HashSet Symbol))
    setsUsingHiding = case uh of
      Just (Using l) -> Just (Left (HashSet.fromList (toList l)))
      Just (Hiding l) -> Just (Right (HashSet.fromList (toList l)))
      Nothing -> Nothing
    shouldOpen :: Symbol -> Bool
    shouldOpen s = case setsUsingHiding of
      Nothing -> True
      Just (Left using) -> HashSet.member s using
      Just (Right hiding) -> not (HashSet.member s hiding)

checkFunctionClause ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  FunctionClause 'Parsed ->
  Sem r (FunctionClause 'Scoped)
checkFunctionClause = undefined

checkAxiom ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  AxiomDef 'Parsed ->
  Sem r (AxiomDef 'Scoped)
checkAxiom = undefined

checkEval ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  Eval 'Parsed ->
  Sem r (Eval 'Scoped)
checkEval (Eval s) = Eval <$> checkParseExpressionSections s

checkPrint ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  Print 'Parsed ->
  Sem r (Print 'Scoped)
checkPrint (Print s) = Print <$> checkParseExpressionSections s

checkFunction ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  Function 'Parsed ->
  Sem r (Function 'Scoped)
checkFunction = undefined

checkLetBlock ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  LetBlock 'Parsed ->
  Sem r (LetBlock 'Scoped)
checkLetBlock = undefined

checkLambda ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  Lambda 'Parsed ->
  Sem r (Lambda 'Scoped)
checkLambda = undefined

checkExpressionSection ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  ExpressionSection 'Parsed ->
  Sem r (ExpressionSection 'Scoped)
checkExpressionSection e = case e of
  SectionIdentifier _ -> undefined
  SectionLambda lam -> SectionLambda <$> checkLambda lam
  SectionLetBlock letBlock -> SectionLetBlock <$> checkLetBlock letBlock
  SectionUniverse uni -> return (SectionUniverse uni)
  SectionFunction fun -> SectionFunction <$> checkFunction fun
  SectionParens par -> SectionParens <$> checkExpressionSections par

checkExpressionSections ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  ExpressionSections 'Parsed ->
  Sem r (ExpressionSections 'Scoped)
checkExpressionSections (ExpressionSections l) = ExpressionSections <$> mapM checkExpressionSection l

checkParseExpressionSections ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  ExpressionSections 'Parsed ->
  Sem r Expression
checkParseExpressionSections = checkExpressionSections >=> parseExpressionSections

checkStatement ::
  Members '[Error ScopeError, Reader ScopeParameters, Embed IO, State ModulesCache, State ModuleCurrentScope] r =>
  Statement 'Parsed ->
  Sem r (Statement 'Scoped)
checkStatement s = case s of
  StatementOperator opDef -> StatementOperator opDef <$ addOperatorSyntaxDef opDef
  StatementTypeSignature tySig -> StatementTypeSignature <$> checkTypeSignature tySig
  StatementImport imp -> StatementImport <$> checkImport imp
  StatementDataType dt -> StatementDataType <$> checkDataTypeDef dt
  StatementModule dt -> StatementModule <$> checkLocalModule dt
  StatementOpenModule open -> StatementOpenModule open <$ checkOpenModule open
  StatementFunctionClause clause -> StatementFunctionClause <$> checkFunctionClause clause
  StatementAxiom ax -> StatementAxiom <$> checkAxiom ax
  StatementEval e -> StatementEval <$> checkEval e
  StatementPrint e -> StatementPrint <$> checkPrint e

-------------------------------------------------------------------------------
-- Infix Parsers
-------------------------------------------------------------------------------

parseExpressionSections ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  ExpressionSections 'Scoped ->
  Sem r Expression
parseExpressionSections = undefined

parsePatternSections ::
  Members '[Error ScopeError, State ModuleCurrentScope] r =>
  PatternSections 'Scoped ->
  Sem r Pattern
parsePatternSections = undefined

parsePatternTerm :: MonadParsec e [PatternSections 'Scoped] m => m Pattern
parsePatternTerm = undefined

parsePatternWildcard :: MonadParsec e [PatternSection 'Scoped] m => m Pattern
parsePatternWildcard = PatternWildcard <$ P.satisfy isWildcard
  where
    isWildcard PatternSectionWildcard = True
    isWildcard _ = False

tmp :: MonadParsec e [PatternSections 'Scoped] m => ModuleCurrentScope -> m Pattern
tmp = undefined
