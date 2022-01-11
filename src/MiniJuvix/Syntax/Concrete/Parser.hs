module MiniJuvix.Syntax.Concrete.Parser where

--------------------------------------------------------------------------------

import qualified Data.List.NonEmpty as NonEmpty
import Data.Singletons
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import MiniJuvix.Syntax.Concrete.Base (MonadParsec)
import qualified MiniJuvix.Syntax.Concrete.Base as P
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Lexer hiding (symbol)
import MiniJuvix.Utils.Prelude hiding (universe)

--------------------------------------------------------------------------------
-- Running the parser
--------------------------------------------------------------------------------

debugModuleParser :: FilePath -> IO ()
debugModuleParser fileName = do
  r <- runModuleParserIO fileName
  case r of
    Left err -> error err
    Right m -> print m

runModuleParserIO :: FilePath -> IO (Either Text (Module 'ModuleTop))
runModuleParserIO fileName = do
  input <- Text.readFile fileName
  return (runModuleParser fileName input)

-- | The 'FilePath' is only used for reporting errors. It is safe to pass
-- an empty string.
runModuleParser :: FilePath -> Text -> Either Text (Module 'ModuleTop)
runModuleParser fileName input =
  case P.runParser topModuleDef fileName input of
    Left err -> Left $ Text.pack (P.errorBundlePretty err)
    Right r -> return r

topModuleDef :: MonadParsec Void Text m => m (Module 'ModuleTop)
topModuleDef = space >> moduleDef <* (optional kwSemicolon >> P.eof)

--------------------------------------------------------------------------------
-- Symbols and names
--------------------------------------------------------------------------------

symbol :: MonadParsec e Text m => m Symbol
symbol = Sym <$> identifier

dottedSymbol :: forall e m. MonadParsec e Text m => m (NonEmpty Symbol)
dottedSymbol = fmap Sym <$> dottedIdentifier

name :: forall e m. MonadParsec e Text m => m Name
name = do
  parts <- dottedSymbol
  return $ case nonEmptyUnsnoc parts of
    (Just p, n) -> NameQualified (QualifiedName (mkModulePath p) n)
    (Nothing, n) -> NameUnqualified n

mkModulePath :: NonEmpty Symbol -> ModulePath
mkModulePath l = ModulePath (NonEmpty.init l) (NonEmpty.last l)

symbolList :: MonadParsec e Text m => m (NonEmpty Symbol)
symbolList = braces (P.sepBy1 symbol kwSemicolon)

modulePath :: MonadParsec e Text m => m ModulePath
modulePath = mkModulePath <$> dottedSymbol

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

statement :: forall e m. MonadParsec e Text m => m Statement
statement =
  (StatementOperator <$> operatorSyntaxDef)
    <|> (StatementOpenModule <$> openModule)
    <|> (StatementEval <$> eval)
    <|> (StatementImport <$> import_)
    <|> (StatementDataType <$> dataTypeDef)
    <|> (StatementPrint <$> printS)
    <|> (StatementModule <$> moduleDef)
    <|> (StatementAxiom <$> axiomDef)
    <|> ( either StatementTypeSignature StatementFunctionClause
            <$> auxTypeSigFunClause
        )

--------------------------------------------------------------------------------
-- Operator syntax declaration
--------------------------------------------------------------------------------

precedence :: MonadParsec e Text m => m Precedence
precedence = decimal

operatorSyntaxDef :: forall e m. MonadParsec e Text m => m OperatorSyntaxDef
operatorSyntaxDef = do
  fixityArity <- arity
  fixityPrecedence <- precedence
  opSymbol <- symbol
  let opFixity = Fixity {..}
  return OperatorSyntaxDef {..}
  where
    arity :: m OperatorArity
    arity =
      do
        Binary AssocRight <$ kwInfixr
        <|> Binary AssocLeft <$ kwInfixl
        <|> Binary AssocNone <$ kwInfix
        <|> Unary AssocPrefix <$ kwPrefix
        <|> Unary AssocPostfix <$ kwPrefix

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

import_ :: MonadParsec e Text m => m Import
import_ = do
  kwImport
  importModule <- modulePath
  return Import {..}

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

expressionSection :: MonadParsec e Text m => m ExpressionSection
expressionSection =
  do
    SectionIdentifier <$> name
    <|> (SectionUniverse <$> universe)
    <|> (SectionLambda <$> lambda)
    <|> (SectionFunction <$> function)
    <|> (SectionMatch <$> match)
    <|> (SectionLetBlock <$> letBlock)
    <|> (SectionFunArrow <$ kwRightArrow)
    <|> parens (SectionParens <$> expressionSections)

expressionSections :: MonadParsec e Text m => m ExpressionSections
expressionSections = ExpressionSections <$> P.some expressionSection

--------------------------------------------------------------------------------
-- Match expression
--------------------------------------------------------------------------------

matchAlt :: MonadParsec e Text m => m MatchAlt
matchAlt = do
  matchAltPattern <- patternSection
  kwMapsTo
  matchAltBody <- expressionSections
  return MatchAlt {..}

match :: MonadParsec e Text m => m (Match)
match = do
  kwMatch
  matchExpression <- expressionSections
  matchAlts <- braces (P.sepEndBy matchAlt kwSemicolon)
  return Match {..}

--------------------------------------------------------------------------------
-- Let expression
--------------------------------------------------------------------------------

letClause :: MonadParsec e Text m => m LetClause
letClause = do
  either LetTypeSig LetFunClause <$> auxTypeSigFunClause

letBlock :: MonadParsec e Text m => m LetBlock
letBlock = do
  kwLet
  letClauses <- braces (P.sepEndBy letClause kwSemicolon)
  return LetBlock {..}

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

defaultUniverse :: Universe
defaultUniverse = Universe 0

universe :: MonadParsec e Text m => m Universe
universe = defaultUniverse <$ kwType

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

typeSignature ::
  forall e m.
  MonadParsec e Text m =>
  Symbol ->
  m (TypeSignature)
typeSignature sigName = do
  kwColon
  sigType <- expressionSections
  return TypeSignature {..}

-------------------------------------------------------------------------------
-- Aux type signature function clause
-------------------------------------------------------------------------------

-- | Used to minimize the amount of required @P.try@s.
auxTypeSigFunClause ::
  forall e m.
  MonadParsec e Text m =>
  m (Either TypeSignature FunctionClause)
auxTypeSigFunClause = do
  s <- symbol
  (Left <$> typeSignature s)
    <|> (Right <$> functionClause s)

-------------------------------------------------------------------------------
-- Axioms
-------------------------------------------------------------------------------

axiomDef :: forall e m. MonadParsec e Text m => m AxiomDef
axiomDef = do
  kwAxiom
  axiomName <- symbol
  kwColon
  axiomType <- expressionSections
  return AxiomDef {..}

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

explicitFunParam ::
  forall e m.
  MonadParsec e Text m =>
  m (FunctionParameter)
explicitFunParam = do
  (paramName, paramUsage) <- P.try $ do
    lparen
    n <- pName
    u <- pUsage
    return (n, u)
  paramType <- expressionSections
  rparen
  return $ FunctionParameter {..}
  where
    pName :: m (Maybe Symbol)
    pName =
      (Just <$> symbol)
        <|> (Nothing <$ kwWildcard)
    pUsage :: m (Maybe Usage)
    pUsage =
      (Just UsageNone <$ kwColonZero)
        <|> (Just UsageOnce <$ kwColonOne)
        <|> (Just UsageOmega <$ kwColonOmega)
        <|> (Nothing <$ kwColon)

functionParam :: MonadParsec e Text m => m FunctionParameter
functionParam = explicitFunParam

function :: MonadParsec e Text m => m Function
function = do
  funParameter <- functionParam
  kwRightArrow
  funReturn <- expressionSections
  return Function {..}

--------------------------------------------------------------------------------
-- Where block clauses
--------------------------------------------------------------------------------

whereBlock :: MonadParsec e Text m => m WhereBlock
whereBlock = do
  kwWhere
  WhereBlock <$> braces (P.sepEndBy whereClause kwSemicolon)

whereClause :: forall e m. MonadParsec e Text m => m WhereClause
whereClause =
  (WhereOpenModule <$> openModule)
    <|> sigOrFun
  where
    sigOrFun :: m (WhereClause)
    sigOrFun = either WhereTypeSig WhereFunClause <$> auxTypeSigFunClause

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

lambdaClause :: MonadParsec e Text m => m LambdaClause
lambdaClause = do
  lambdaParameters <- P.some patternSection
  kwMapsTo
  lambdaBody <- expressionSections
  return LambdaClause {..}


lambda :: MonadParsec e Text m => m Lambda
lambda = do
  kwLambda
  lambdaClauses ← braces (P.sepEndBy lambdaClause kwSemicolon)
  return Lambda {..}

-------------------------------------------------------------------------------
-- Data type construction declaration
-------------------------------------------------------------------------------

dataTypeDef :: MonadParsec e Text m => m DataTypeDef
dataTypeDef = do
  kwInductive
  dataTypeName <- symbol
  dataTypeParameters <- P.many dataTypeParam
  dataTypeType <- optional (kwColon >> expressionSections)
  dataTypeConstructors <- braces $ P.sepEndBy constructorDef kwSemicolon
  return DataTypeDef {..}

dataTypeParam :: MonadParsec e Text m => m DataTypeParameter
dataTypeParam = parens $ do
  dataTypeParameterName <- symbol
  kwColon
  dataTypeParameterType <- expressionSections
  return DataTypeParameter {..}

constructorDef :: MonadParsec e Text m => m DataConstructorDef
constructorDef = do
  constructorName <- symbol
  kwColon
  constructorType <- expressionSections
  return DataConstructorDef {..}

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

patternSection :: forall e m. MonadParsec e Text m => m PatternSection
patternSection =
  PatternSectionName <$> name
    <|> PatternSectionWildcard <$ kwWildcard
    <|> (PatternSectionParen <$> parens patternSections)

patternSections ::
  forall e m.
  MonadParsec e Text m =>
  m (PatternSections)
patternSections = PatternSections <$> P.some patternSection

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

functionClause ::
  forall e m.
  MonadParsec e Text m =>
  Symbol ->
  m (FunctionClause)
functionClause clauseOwnerFunction = do
  clausePatterns <- P.many patternSection
  kwAssignment
  clauseBody <- expressionSections
  clauseWhere <- optional whereBlock
  return FunClause {..}

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

pmoduleModulePath ::
  forall t e m.
  (SingI t, MonadParsec e Text m) =>
  m (ModulePathType t)
pmoduleModulePath = case sing :: SModuleIsTop t of
  SModuleTop -> modulePath
  SModuleLocal -> symbol

moduleDef :: (SingI t, MonadParsec e Text m) => m (Module t)
moduleDef = do
  kwModule
  moduleModulePath <- pmoduleModulePath
  kwSemicolon
  moduleBody <- P.sepEndBy statement kwSemicolon
  kwEnd
  return Module {..}

openModule :: forall e m. MonadParsec e Text m => m OpenModule
openModule = do
  kwOpen
  openModuleName <- modulePath
  openUsingHiding <- optional usingOrHiding
  return OpenModule {..}
  where
    usingOrHiding :: m UsingHiding
    usingOrHiding =
      (kwUsing >> (Using <$> symbolList))
        <|> (kwHiding >> (Hiding <$> symbolList))

--------------------------------------------------------------------------------
-- Debugging statements
--------------------------------------------------------------------------------

eval :: MonadParsec e Text m => m Eval
eval = do
  kwEval
  Eval <$> expressionSections

printS :: MonadParsec e Text m => m Print
printS = do
  kwPrint
  Print <$> expressionSections
