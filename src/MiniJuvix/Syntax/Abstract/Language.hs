{-# LANGUAGE TemplateHaskell #-}
module MiniJuvix.Syntax.Abstract.Language (
  module MiniJuvix.Syntax.Abstract.Language
                                          ) where

import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import MiniJuvix.Syntax.Concrete.Language (Usage)
import qualified MiniJuvix.Syntax.Concrete.Name as C

type TopModuleName = S.TopModulePath
type LocalModuleName = S.Symbol
type FunctionName = S.Symbol
type VarName = S.Symbol
type ConstrName = S.Symbol
type InductiveName = S.Symbol
type Name = S.Name

type TopModule = Module C.TopModulePath
type LocalModule = Module C.Symbol

data Module s = Module
  { _moduleName :: S.Name' s,
    _moduleBody :: [Statement]
  }
  deriving stock (Show, Eq)

data Statement =
  StatementAxiom
  | StatementInductive InductiveDef
  | StatementFunctionDef FunctionDef
  | StatementModule LocalModule
  deriving stock (Show, Eq)

data FunctionDef = FunctionDef {
   _funDefName :: FunctionName,
   _funDefTypeSig :: Expression,
   _funDefClauses :: NonEmpty FunctionClause
  }
  deriving stock (Show, Eq)
data FunctionClause = FunctionClause {
    _clausePatterns :: [Pattern],
    _clauseBody :: Expression
  }
  deriving stock (Show, Eq)

data Iden = 
  IdenDefined Name
  | IdenConstructor Name
  | IdenVar VarName
  | IdenInductive Name
  | IdenAxiom Name
  deriving stock (Show, Eq)

data Expression
  = ExpressionIden Iden
  | ExpressionApplication Application
  | ExpressionUniverse Universe
  | ExpressionFunction Function

  --- | ExpressionMatch Match
  ---  ExpressionLambda Lambda not supported yet
  deriving stock (Show, Eq)

data Match = Match
  { _matchExpression :: Expression,
    _matchAlts :: [MatchAlt]
  }
  deriving stock (Show, Eq)


data MatchAlt = MatchAlt
  { _matchAltPattern :: Pattern,
    _matchAltBody :: Expression
  }
  deriving stock (Show, Eq)

newtype Universe = Universe {
  _universeLevel :: Maybe Natural
  }
  deriving stock (Show, Eq)

data Application = Application {
  _appLeft :: Expression,
  _appRight :: Expression
  }
  deriving stock (Show, Eq)

newtype Lambda = Lambda
  {_lambdaClauses :: [LambdaClause]}
  deriving stock (Show, Eq)

data LambdaClause = LambdaClause
  { _lambdaParameters :: NonEmpty Pattern,
    _lambdaBody :: Expression
  }
  deriving stock (Show, Eq)

data FunctionParameter = FunctionParameter
  { _paramName :: Maybe VarName,
    _paramUsage :: Usage,
    _paramType :: Expression
  }
  deriving stock (Show, Eq)

data Function = Function
  { _funParameter :: FunctionParameter,
    _funReturn :: Expression
  }
  deriving stock (Show, Eq)

-- | Fully applied constructor in a pattern.
data ConstructorApp = ConstructorApp {
  _constrAppConstructor :: Name,
  _constrAppParameters :: [Pattern]
  }
  deriving stock (Show, Eq)

data Pattern
  = PatternVariable VarName
  | PatternConstructorApp ConstructorApp
  | PatternWildcard
  | PatternEmpty
  deriving stock (Show, Eq)

data InductiveDef = InductiveDef
  { _inductiveName :: InductiveName,
    _inductiveParameters :: [FunctionParameter],
    _inductiveType :: Expression,
    _inductiveConstructors :: [InductiveConstructorDef]
  }
  deriving stock (Show, Eq)

data InductiveConstructorDef = InductiveConstructorDef
  { _constructorName :: ConstrName,
    _constructorType :: Expression
  }
  deriving stock (Show, Eq)

makeLenses ''Module
makeLenses ''FunctionParameter
makeLenses ''Function
makeLenses ''FunctionDef
makeLenses ''FunctionClause
makeLenses ''InductiveDef
makeLenses ''InductiveConstructorDef
