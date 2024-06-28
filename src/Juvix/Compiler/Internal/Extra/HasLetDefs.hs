module Juvix.Compiler.Internal.Extra.HasLetDefs where

import Juvix.Compiler.Internal.Language
import Juvix.Prelude

class HasLetDefs a where
  letDefs' :: [Let] -> a -> [Let]
  letDefs :: a -> [Let]
  letDefs = letDefs' []

instance (HasLetDefs a, Foldable f) => HasLetDefs (f a) where
  letDefs' = foldl' letDefs'

instance HasLetDefs Expression where
  letDefs' acc = \case
    ExpressionIden {} -> acc
    ExpressionApplication x -> letDefs' acc x
    ExpressionFunction x -> letDefs' acc x
    ExpressionLiteral {} -> acc
    ExpressionHole {} -> acc
    ExpressionInstanceHole {} -> acc
    ExpressionLet x -> letDefs' acc x
    ExpressionUniverse {} -> acc
    ExpressionSimpleLambda x -> letDefs' acc x
    ExpressionLambda x -> letDefs' acc x
    ExpressionCase x -> letDefs' acc x

instance HasLetDefs Application where
  letDefs' acc Application {..} = letDefs' (letDefs' acc _appLeft) _appRight

instance HasLetDefs Function where
  letDefs' acc Function {..} = letDefs' (letDefs' acc _functionLeft) _functionRight

instance HasLetDefs FunctionParameter where
  letDefs' acc FunctionParameter {..} = letDefs' acc _paramType

instance HasLetDefs Let where
  letDefs' acc x@Let {..} = x : letDefs' (letDefs' acc _letExpression) _letClauses

instance HasLetDefs LetClause where
  letDefs' acc = \case
    LetFunDef x -> letDefs' acc x
    LetMutualBlock x -> letDefs' acc x

instance HasLetDefs SimpleLambda where
  letDefs' acc SimpleLambda {..} = letDefs' (letDefs' acc _slambdaBinder) _slambdaBody

instance HasLetDefs SimpleBinder where
  letDefs' acc SimpleBinder {..} = letDefs' acc _sbinderType

instance HasLetDefs Lambda where
  letDefs' acc Lambda {..} = letDefs' (letDefs' acc _lambdaType) _lambdaClauses

instance HasLetDefs LambdaClause where
  letDefs' acc LambdaClause {..} = letDefs' (letDefs' acc _lambdaBody) _lambdaPatterns

instance HasLetDefs PatternArg where
  letDefs' acc PatternArg {..} = letDefs' acc _patternArgPattern

instance HasLetDefs Pattern where
  letDefs' acc = \case
    PatternVariable {} -> acc
    PatternConstructorApp x -> letDefs' acc x
    PatternWildcardConstructor {} -> acc

instance HasLetDefs ConstructorApp where
  letDefs' acc ConstructorApp {..} = letDefs' (letDefs' acc _constrAppType) _constrAppParameters

instance HasLetDefs Case where
  letDefs' acc Case {..} = letDefs' (letDefs' acc _caseExpression) _caseBranches

instance HasLetDefs CaseBranch where
  letDefs' acc CaseBranch {..} = letDefs' acc _caseBranchExpression

instance HasLetDefs MutualBlockLet where
  letDefs' acc MutualBlockLet {..} = letDefs' acc _mutualLet

instance HasLetDefs MutualBlock where
  letDefs' acc MutualBlock {..} = letDefs' acc _mutualStatements

instance HasLetDefs MutualStatement where
  letDefs' acc = \case
    StatementInductive x -> letDefs' acc x
    StatementFunction x -> letDefs' acc x
    StatementAxiom x -> letDefs' acc x

instance HasLetDefs InductiveDef where
  letDefs' acc InductiveDef {..} = letDefs' (letDefs' (letDefs' acc _inductiveType) _inductiveConstructors) _inductiveParameters

instance HasLetDefs InductiveParameter where
  letDefs' acc InductiveParameter {..} = letDefs' acc _inductiveParamType

instance HasLetDefs ConstructorDef where
  letDefs' acc ConstructorDef {..} = letDefs' acc _inductiveConstructorType

instance HasLetDefs FunctionDef where
  letDefs' acc FunctionDef {..} = letDefs' (letDefs' (letDefs' acc _funDefType) _funDefBody) _funDefArgsInfo

instance HasLetDefs ArgInfo where
  letDefs' acc ArgInfo {..} = letDefs' acc _argInfoDefault

instance HasLetDefs AxiomDef where
  letDefs' acc AxiomDef {..} = letDefs' acc _axiomType
