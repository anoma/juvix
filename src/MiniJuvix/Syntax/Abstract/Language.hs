module MiniJuvix.Syntax.Abstract.Language where


import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Name

type ModuleName = NameId
type FunctionName = NameId
type VarName = NameId
type ConstrName = NameId

data Module = Module
  { moduleName :: ModuleName,
    moduleBody :: [Statement]
  }
  deriving stock (Show, Eq)

data Statement =
  StatementAxiom
  | StatementInductive
  | StatementFunctionDef FunctionDef
  | StatementModule Module
  deriving stock (Show, Eq)

data FunctionDef = FunctionDef {
   funDefName :: FunctionName,
   funDefTypeSig :: Expression,
   funDefClauses :: NonEmpty FunctionClause
  }
  deriving stock (Show, Eq)

data FunctionClause = FunctionClause {
    clausePatterns :: [Pattern],
    clauseBody :: Expression
  }
  deriving stock (Show, Eq)

data Expression
  = ExpressionVar VarName
  | ExpressionConstructor ConstrName
  | ExpressionApplication Application
  | ExpressionUniverse Universe
  | ExpressionFunction Function
  | ExpressionMatch Match
  --  ExpressionLambda Lambda not supported yet
  deriving stock (Show, Eq)

data Match = Match
  { matchExpression :: Expression,
    matchAlts :: [MatchAlt]
  }
  deriving stock (Show, Eq)


data MatchAlt = MatchAlt
  { matchAltPattern :: Pattern,
    matchAltBody :: Expression
  }
  deriving stock (Show, Eq)

newtype Universe = Universe {
  _universeLevel :: Int
  }
  deriving stock (Show, Eq)

data Application = Application {
  appLeft :: Expression,
  appRight :: Expression
  }
  deriving stock (Show, Eq)

newtype Lambda = Lambda
  {lambdaClauses :: [LambdaClause]}
  deriving stock (Show, Eq)

data LambdaClause = LambdaClause
  { lambdaParameters :: NonEmpty Pattern,
    lambdaBody :: Expression
  }
  deriving stock (Show, Eq)

data Usage
  = UsageNone
  | UsageOnce
  | UsageOmega
  deriving stock (Show, Eq)

data FunctionParameter = FunctionParameter
  { paramName :: Maybe NameId,
    paramUsage :: Usage,
    paramType :: Expression
  }
  deriving stock (Show, Eq)

data Function = Function
  { funParameter :: FunctionParameter,
    funReturn :: Expression
  }
  deriving stock (Show, Eq)

-- | Fully applied constructor in a pattern.
data ConstructorApp = ConstructorApp {
  constrAppConstructor :: ConstrName,
  constrAppParameters :: [Pattern]
  }
  deriving stock (Show, Eq)

data Pattern
  = PatternVariable VarName
  | PatternConstructorApp ConstructorApp
  | PatternWildcard
  | PatternEmpty
  deriving stock (Show, Eq)
