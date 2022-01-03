module MiniJuvix.Parsing.Parser where

import MiniJuvix.Parsing.Language
import MiniJuvix.Utils.Prelude hiding (universe)
import MiniJuvix.Parsing.Lexer hiding (symbol)
import qualified MiniJuvix.Parsing.Base as P
import MiniJuvix.Parsing.Base (MonadParsec)
import qualified Data.List.NonEmpty as NonEmpty

topModuleDef ∷ MonadParsec e Text m ⇒ m (Module 'Parsed)
topModuleDef = space >> moduleDef <* P.eof

--------------------------------------------------------------------------------
-- Symbols and names
--------------------------------------------------------------------------------

symbol ∷ MonadParsec e Text m ⇒ m Symbol
symbol = Sym <$> identifier

dottedSymbol ∷ ∀ e m. MonadParsec e Text m ⇒ m (NonEmpty Symbol)
dottedSymbol = fmap Sym <$> dottedIdentifier

name ∷ ∀ e m. MonadParsec e Text m ⇒ m Name
name = do
  parts ← dottedSymbol
  return $ case nonEmptyUnsnoc parts of
    (Just p, n) → QualifiedName (Qualified (mkModulePath p) n)
    (Nothing, n) → Unqualified n

mkModulePath ∷ NonEmpty Symbol → ModulePath
mkModulePath l = ModulePath (NonEmpty.init l) (NonEmpty.last l)

symbolList ∷ MonadParsec e Text m ⇒ m (NonEmpty Symbol)
symbolList = braces (P.sepBy1 symbol kwSemicolon)

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

statement ∷ ∀ e m. MonadParsec e Text m ⇒ m (Statement 'Parsed)
statement =
  (StatementOperator <$> operatorSyntaxDef)
  <|> (StatementOpenModule <$> openModule)
  <|> (StatementEval <$> eval)
  <|> (StatementImport <$> import_)
  <|> (StatementDataType <$> dataTypeDef)
  <|> (StatementPrint <$> printS)
  <|> (StatementModule <$> moduleDef)
  <|> (StatementAxiom <$> axiomDef)
  <|> (either StatementTypeSignature StatementFunctionClause <$> auxTypeSigFunClause)

--------------------------------------------------------------------------------
-- Operator syntax declaration
--------------------------------------------------------------------------------

precedence ∷ MonadParsec e Text m ⇒ m Precedence
precedence = decimal

operatorSyntaxDef ∷ ∀ e m. MonadParsec e Text m ⇒ m OperatorSyntaxDef
operatorSyntaxDef = do
  opArity ← arity
  opPrecedence ← precedence
  opSymbol ← symbol
  return OperatorSyntaxDef {..}
  where
  arity ∷ m OperatorArity
  arity = do
    Binary AssocRight <$ kwInfixr
    <|> Binary AssocLeft <$ kwInfixl
    <|> Binary AssocNone <$ kwInfix
    <|> Unary AssocPrefix <$ kwPrefix
    <|> Unary AssocPostfix <$ kwPrefix

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

import_ ∷ MonadParsec e Text m ⇒ m Import
import_ = do
  kwImport
  importModule ← mkModulePath <$> dottedSymbol
  return Import {..}

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

expressionSection ∷ MonadParsec e Text m ⇒ m (ExpressionSection 'Parsed)
expressionSection = do
  SectionIdentifier <$> name
  <|> (SectionUniverse <$> universe)
  <|> (SectionLambda <$> lambda)
  <|> parens (SectionParens <$> expressionSections)

expressionSections ∷ MonadParsec e Text m ⇒ m (ExpressionSections 'Parsed)
expressionSections = ExpressionSections <$> P.some expressionSection

--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

defaultUniverse ∷ Universe
defaultUniverse = Universe 0

universe ∷ MonadParsec e Text m ⇒ m Universe
universe = defaultUniverse <$ kwType

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

typeSignature ∷ ∀ e m. MonadParsec e Text m ⇒ Symbol → m (TypeSignature 'Parsed)
typeSignature sigName = do
  kwColon
  sigType ← expressionSections
  return TypeSignature{..}

-------------------------------------------------------------------------------
-- Aux type signature function clause
-------------------------------------------------------------------------------

-- | Used to minimize the amount of required @P.try@s.
auxTypeSigFunClause ∷ ∀ e m. MonadParsec e Text m
  ⇒ m (Either (TypeSignature 'Parsed) (FunctionClause 'Parsed))
auxTypeSigFunClause = do
  s ← symbol
  (Left <$> typeSignature s)
   <|> (Right <$> functionClause s)

-------------------------------------------------------------------------------
-- Axioms
-------------------------------------------------------------------------------

axiomDef ∷ ∀ e m. MonadParsec e Text m ⇒ m (AxiomDef 'Parsed)
axiomDef = do
  kwAxiom
  axiomName ← symbol
  kwColon
  axiomType ← expressionSections
  return AxiomDef{..}

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

explicitFunParam ∷ ∀ e m. MonadParsec e Text m ⇒ m (FunctionParameter 'Parsed)
explicitFunParam = parens $ do
  paramName ← pName
  paramUsage ← pUsage
  paramType ← expressionSections
  return $ FunctionParameter {..}
  where
  pName ∷ m (Maybe Symbol)
  pName = (Just <$> symbol)
    <|> (Nothing <$ kwWildcard)
  pUsage ∷ m (Maybe Usage)
  pUsage =
    (Just UsageNone <$ kwColonZero)
    <|> (Just UsageOnce <$ kwColonOne)
    <|> (Just UsageOmega <$ kwColonOmega)
    <|> (Nothing <$ kwColon)

functionParam ∷ MonadParsec e Text m ⇒ m (FunctionParameter 'Parsed)
functionParam = do
  sec ← P.try (P.optional explicitFunParam)
  case sec of
    Just p → return p
    Nothing → do
      ty ← expressionSections
      return FunctionParameter {
        paramName = Nothing,
        paramUsage = Nothing,
        paramType = ty
        }

function ∷ MonadParsec e Text m ⇒ m (Function 'Parsed)
function = do
  funParameter ← functionParam
  kwArrowR
  funReturn ← expressionSections
  return Function{..}

--------------------------------------------------------------------------------
-- Where block clauses
--------------------------------------------------------------------------------

whereBlock ∷ MonadParsec e Text m ⇒ m (WhereBlock 'Parsed)
whereBlock = do
  kwWhere
  WhereBlock <$> P.sepBy whereClause kwSemicolon

whereClause ∷ ∀ e m. MonadParsec e Text m ⇒ m (WhereClause 'Parsed)
whereClause =
  (WhereOpenModule <$> openModule)
  <|> sigOrFun
  where
  sigOrFun ∷ m (WhereClause 'Parsed)
  sigOrFun = either WhereTypeSig WhereFunClause <$> auxTypeSigFunClause

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

lambda ∷ MonadParsec e Text m ⇒ m (Lambda 'Parsed)
lambda = do
  kwLambda
  lambdaParameters ← P.some patternSection
  kwArrowR
  lambdaBody ← expressionSections
  return Lambda{..}

-------------------------------------------------------------------------------
-- Data type construction declaration
-------------------------------------------------------------------------------

dataTypeDef ∷ MonadParsec e Text m ⇒ m (DataTypeDef 'Parsed)
dataTypeDef = do
  kwInductive
  dataTypeName ← symbol
  dataTypeParameters ← P.many dataTypeParam
  dataTypeType ← optional (kwColon >> expressionSections)
  dataTypeConstructors ← braces $ P.sepBy constructorDef kwSemicolon
  return DataTypeDef {..}

dataTypeParam ∷ MonadParsec e Text m ⇒ m (DataTypeParameter 'Parsed)
dataTypeParam = parens $ do
  dataTypeParameterName ← symbol
  dataTypeParameterType ← expressionSections
  return DataTypeParameter {..}

constructorDef ∷ MonadParsec e Text m ⇒ m (DataConstructorDef 'Parsed)
constructorDef = do
  constructorName ← symbol
  kwColon
  constructorType ← expressionSections
  return DataConstructorDef {..}

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

patternSection ∷ ∀ e m. MonadParsec e Text m ⇒ m (PatternSection 'Parsed)
patternSection =
  PatternSectionVariable <$> symbol
  <|> PatternSectionWildcard <$ kwWildcard
  <|> (PatternSectionParen <$> parens patternSections)

patternSections ∷ ∀ e m. MonadParsec e Text m ⇒ m (PatternSections 'Parsed)
patternSections = PatternSections <$> P.some patternSection

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

functionClause ∷ ∀ e m. MonadParsec e Text m ⇒ Symbol → m (FunctionClause 'Parsed)
functionClause clauseOwnerFunction = do
  clausePatterns ← P.many patternSection
  kwDef
  clauseBody ← expressionSections
  clauseWhere ← optional whereBlock
  return FunClause{..}

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

moduleDef ∷ MonadParsec e Text m ⇒ m (Module 'Parsed)
moduleDef = do
  kwModule
  moduleName ← symbol
  moduleBody ← P.sepBy statement kwSemicolon
  kwEnd
  return Module{..}

openModule ∷ ∀ e m. MonadParsec e Text m ⇒ m OpenModule
openModule = do
  kwOpen
  openModuleName ← symbol
  r ← optional usingOrHiding
  let (openUsing, openHiding) = case r of
        Nothing → (Nothing, Nothing)
        Just (Left using) → (Just using, Nothing)
        Just (Right hiding) → (Nothing, Just hiding)
  return OpenModule {..}
  where
  usingOrHiding ∷ m (Either (NonEmpty Symbol) (NonEmpty Symbol))
  usingOrHiding =
    (kwUsing >> (Left <$> symbolList))
    <|> (kwHiding >> (Right <$> symbolList))

--------------------------------------------------------------------------------
-- Debugging statements
--------------------------------------------------------------------------------

eval ∷ MonadParsec e Text m ⇒ m (Eval 'Parsed)
eval = do
  kwEval
  Eval <$> expressionSections

printS ∷ MonadParsec e Text m ⇒ m (Print 'Parsed)
printS = do
  kwPrint
  Print <$> expressionSections
