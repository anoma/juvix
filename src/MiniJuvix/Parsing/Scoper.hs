module MiniJuvix.Parsing.Scoper where

import MiniJuvix.Parsing.Language
import qualified MiniJuvix.Parsing.Base as P
import MiniJuvix.Parsing.Base (MonadParsec)
import MiniJuvix.Parsing.Parser (runModuleParserIO)
import qualified Data.List.NonEmpty as NonEmpty
import MiniJuvix.Utils.Prelude hiding (State, get, put, modify, Reader, ask, asks)
import qualified Data.Kind as GHC
import Polysemy
import Polysemy.Error         hiding (fromEither)
import Polysemy.State
import Polysemy.Reader
import Polysemy.Embed
import System.FilePath
import qualified Data.Text as Text

-- | Relevant scope information of a module.
data ModuleScopeInfo = ModuleScopeInfo {
  -- | Operator definitions. They can refer to either constructors or functions.
  syntaxOperators ∷ [OperatorSyntaxDef],
  -- | constructors introduced by inductive definitions (E.g. ℕ; zero; suc).
  syntaxConstructors ∷ HashSet DataConstructorName,
  -- | function names in scope. Function names are introduced with function clauses.
  syntaxFunctions ∷ HashSet FunctionName,
  -- | locally defined modules. Imported modules are not included.
  syntaxModules ∷ HashMap Symbol ModuleScopeInfo
  }

newtype IdentifierInfo = IdentifierInfo {
  idenInfoOrigins ∷ HashSet ModulePath
  }

data ModuleCurrentScope = ModuleGlobalScope {
  currentOperators ∷ [OperatorSyntaxDef],
  currentConstructors ∷ HashMap DataConstructorName IdentifierInfo,
  currentFunctions ∷ HashMap FunctionName IdentifierInfo,
  currentImported ∷ HashMap ModulePath ModuleScopeInfo
  }

newtype LocalScope = LocalScope {
  localScopeSymbols ∷ HashSet Symbol
  }

newtype PatternScope = PatternScope {
  patternScopeSymbols ∷ HashSet Symbol
  }

data ScopeError =
  ParseError Text
  | Err
  deriving stock (Show)

data ScopeParameters = ScopParameters {
  scopeRootPath ∷ FilePath,
  scopeExtension ∷ FilePath
  }

scopeCheck ∷ FilePath → Module 'Parsed → Either ScopeError ModuleScopeInfo
scopeCheck root m = undefined

checkImport ∷ Members '[Error ScopeError, State ModuleScopeInfo, Reader ScopeParameters, Embed IO] r ⇒
  Import → Sem r Import
checkImport (Import p) = do
  par ← readParseModule p
  undefined

readParseModule ∷ Members '[Error ScopeError, Reader ScopeParameters, Embed IO] r ⇒
  ModulePath → Sem r (Module 'Parsed)
readParseModule mp = do
  path ← modulePathToFilePath mp
  res ← embed (runModuleParserIO path)
  case res of
    Left err → throw (ParseError err)
    Right r → return r

modulePathToFilePath ∷ Members '[Reader ScopeParameters] r ⇒
  ModulePath → Sem r FilePath
modulePathToFilePath mp = do
  root ← asks scopeRootPath
  ext ← asks scopeExtension
  let relDirPath = foldr ((</>) . toPath) mempty (modulePathDir mp)
      relFilePath = relDirPath </> toPath (modulePathName mp) <.> ext
  return $ root </> relFilePath
  where
  toPath ∷ Symbol → FilePath
  toPath (Sym t) = Text.unpack t

addOperatorSyntaxDef ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  OperatorSyntaxDef → Sem r ()
addOperatorSyntaxDef = undefined

checkTypeSignature ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  TypeSignature 'Parsed → Sem r (TypeSignature 'Scoped)
checkTypeSignature = undefined

addImport ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  Import → Sem r ()
addImport = undefined

checkDataTypeDef ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  DataTypeDef 'Parsed → Sem r (DataTypeDef 'Scoped)
checkDataTypeDef = undefined

checkModule ∷ Members '[Error ScopeError, State ModuleScopeInfo, Reader ScopeParameters] r ⇒
  Module 'Parsed → Sem r (Module 'Scoped)
checkModule = undefined

checkOpenModule ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  OpenModule → Sem r ()
checkOpenModule = undefined

checkFunctionClause ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  FunctionClause 'Parsed → Sem r (FunctionClause 'Scoped)
checkFunctionClause = undefined

checkAxiom ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  AxiomDef 'Parsed → Sem r (AxiomDef 'Scoped)
checkAxiom = undefined

checkEval ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  Eval 'Parsed → Sem r (Eval 'Scoped)
checkEval (Eval s) = Eval <$> checkParseExpressionSections s

checkPrint ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  Print 'Parsed → Sem r (Print 'Scoped)
checkPrint (Print s) = Print <$> checkParseExpressionSections s

checkFunction ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  Function 'Parsed → Sem r (Function 'Scoped)
checkFunction = undefined

checkLetBlock ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  LetBlock 'Parsed → Sem r (LetBlock 'Scoped)
checkLetBlock = undefined

checkLambda ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  Lambda 'Parsed → Sem r (Lambda 'Scoped)
checkLambda = undefined

checkExpressionSection ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  ExpressionSection 'Parsed → Sem r (ExpressionSection 'Scoped)
checkExpressionSection e = case e of
  SectionIdentifier _ → undefined
  SectionLambda lam → SectionLambda <$> checkLambda lam
  SectionLetBlock letBlock → SectionLetBlock <$> checkLetBlock letBlock
  SectionUniverse uni → return (SectionUniverse uni)
  SectionFunction fun → SectionFunction <$> checkFunction fun
  SectionParens par → SectionParens <$> checkExpressionSections par

checkExpressionSections ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  ExpressionSections 'Parsed → Sem r (ExpressionSections 'Scoped)
checkExpressionSections (ExpressionSections l) = ExpressionSections <$> mapM checkExpressionSection l

checkParseExpressionSections ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  ExpressionSections 'Parsed → Sem r Expression
checkParseExpressionSections = checkExpressionSections >=> parseExpressionSections

checkStatement ∷ Members '[Error ScopeError, State ModuleScopeInfo, Reader ScopeParameters, Embed IO] r
  ⇒ Statement 'Parsed → Sem r (Statement 'Scoped)
checkStatement s = case s of
  StatementOperator opDef → StatementOperator opDef <$ addOperatorSyntaxDef opDef
  StatementTypeSignature tySig → StatementTypeSignature <$> checkTypeSignature tySig
  StatementImport imp → StatementImport imp <$ addImport imp
  StatementDataType dt → StatementDataType <$> checkDataTypeDef dt
  StatementModule dt → StatementModule <$> checkModule dt
  StatementOpenModule open → StatementOpenModule open <$ checkOpenModule open
  StatementFunctionClause clause → StatementFunctionClause <$> checkFunctionClause clause
  StatementAxiom ax → StatementAxiom <$> checkAxiom ax
  StatementEval e → StatementEval <$> checkEval e
  StatementPrint e → StatementPrint <$> checkPrint e

-------------------------------------------------------------------------------
-- Infix Parsers
-------------------------------------------------------------------------------

parseExpressionSections ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  ExpressionSections 'Scoped → Sem r Expression
parseExpressionSections = undefined

parsePatternSections ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  PatternSections 'Scoped → Sem r Pattern
parsePatternSections = undefined


parsePatternTerm ∷ MonadParsec e [PatternSections 'Scoped] m ⇒ m Pattern
parsePatternTerm = undefined

parsePatternWildcard ∷ MonadParsec e [PatternSection 'Scoped] m ⇒ m Pattern
parsePatternWildcard = PatternWildcard <$ P.satisfy isWildcard
  where isWildcard PatternSectionWildcard = True
        isWildcard _ = False

tmp ∷ MonadParsec e [PatternSections 'Scoped] m ⇒ ModuleCurrentScope → m Pattern
tmp = undefined
