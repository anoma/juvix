module MiniJuvix.Parsing.Scoper where

import MiniJuvix.Parsing.Language
import qualified MiniJuvix.Parsing.Base as P
import MiniJuvix.Parsing.Base (MonadParsec)
import qualified Data.List.NonEmpty as NonEmpty
import MiniJuvix.Utils.Prelude hiding (State, get, put)
import qualified Data.Kind as GHC
import           Polysemy
import           Polysemy.Error         hiding (fromEither)
import           Polysemy.State


data ModuleScopeInfo = ModuleScopeInfo {
  syntaxOperators ∷ [OperatorSyntaxDef],
  syntaxDataConstructors ∷ HashSet DataConstructorName,
  syntaxFunctions ∷ HashSet FunctionName,
  syntaxModules ∷ HashMap Symbol ModuleScopeInfo
  }

data ModuleCurrentScope = ModuleGlobalScope {
  }

newtype LocalScope = LocalScope {
  localScopeSymbols ∷ HashSet Symbol
  }

newtype PatternScope = PatternScope {
  patternScopeSymbols ∷ HashSet Symbol
  }


data ScopeError = DupOperatorDef
  deriving stock (Show)

scopeCheck ∷ Module 'Parsed → Either ScopeError ModuleScopeInfo
scopeCheck = undefined

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

checkModule ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
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

parseExpressionSections ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  ExpressionSections 'Scoped → Sem r Expression
parseExpressionSections = undefined

checkParseExpressionSections ∷ Members '[Error ScopeError, State ModuleScopeInfo] r ⇒
  ExpressionSections 'Parsed → Sem r Expression
checkParseExpressionSections = checkExpressionSections >=> parseExpressionSections

checkStatement ∷ Members '[Error ScopeError, State ModuleScopeInfo] r
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
