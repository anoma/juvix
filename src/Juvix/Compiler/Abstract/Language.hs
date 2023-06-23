module Juvix.Compiler.Abstract.Language
  ( module Juvix.Compiler.Abstract.Language,
    module Juvix.Compiler.Concrete.Language,
    module Juvix.Data.Hole,
    module Juvix.Compiler.Concrete.Data.Builtins,
    module Juvix.Compiler.Concrete.Data.Literal,
    module Juvix.Data.Universe,
    module Juvix.Compiler.Abstract.Data.Name,
    module Juvix.Data.Wildcard,
    module Juvix.Data.IsImplicit,
  )
where

import Juvix.Compiler.Abstract.Data.Name
import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Compiler.Concrete.Data.Literal
import Juvix.Compiler.Concrete.Language (symbolLoc)
import Juvix.Data.Hole
import Juvix.Data.IsImplicit
import Juvix.Data.Universe
import Juvix.Data.Wildcard
import Juvix.Prelude

type LocalModule = Module

type TopModule = Module

type TopModuleName = Name

data Module = Module
  { _moduleName :: Name,
    _moduleExamples :: [Example],
    _moduleBody :: ModuleBody,
    _modulePragmas :: Pragmas
  }
  deriving stock (Eq, Show)

newtype ModuleBody = ModuleBody
  { _moduleStatements :: [Statement]
  }
  deriving stock (Eq, Show)

newtype Include = Include
  { _includeModule :: Module
  }
  deriving stock (Show, Eq)

data Statement
  = StatementInductive InductiveDef
  | StatementFunction FunctionDef
  | StatementInclude Include
  | StatementLocalModule LocalModule
  | StatementAxiom AxiomDef
  deriving stock (Eq, Show)

data Example = Example
  { _exampleId :: NameId,
    _exampleExpression :: Expression
  }
  deriving stock (Eq, Show)

data FunctionDef = FunctionDef
  { _funDefName :: FunctionName,
    _funDefTypeSig :: Expression,
    _funDefExamples :: [Example],
    _funDefClauses :: NonEmpty FunctionClause,
    _funDefBuiltin :: Maybe BuiltinFunction,
    _funDefTerminating :: Bool,
    _funDefPragmas :: Pragmas
  }
  deriving stock (Eq, Show)

data FunctionClause = FunctionClause
  { _clauseName :: FunctionName,
    _clausePatterns :: [PatternArg],
    _clauseBody :: Expression
  }
  deriving stock (Eq, Show)

data Iden
  = IdenFunction FunctionName
  | IdenConstructor ConstrName
  | IdenVar VarName
  | IdenInductive InductiveName
  | IdenAxiom AxiomName
  deriving stock (Eq, Show)

data CaseBranch = CaseBranch
  { _caseBranchPattern :: PatternArg,
    _caseBranchExpression :: Expression
  }
  deriving stock (Eq, Show)

data Case = Case
  { _caseExpression :: Expression,
    _caseBranches :: NonEmpty CaseBranch,
    _caseParens :: Bool
  }
  deriving stock (Eq, Show)

newtype LetClause
  = LetFunDef FunctionDef
  deriving stock (Eq, Show)

data Let = Let
  { _letClauses :: NonEmpty LetClause,
    _letExpression :: Expression
  }
  deriving stock (Eq, Show)

data Expression
  = ExpressionIden Iden
  | ExpressionApplication Application
  | ExpressionFunction Function
  | ExpressionLiteral LiteralLoc
  | ExpressionHole Hole
  | ExpressionLet Let
  | ExpressionUniverse SmallUniverse
  | ExpressionLambda Lambda
  | -- | ExpressionSimpleLambda SimpleLambda -- NOTE note needed
    ExpressionCase Case
  deriving stock (Eq, Show)

data Application = Application
  { _appLeft :: Expression,
    _appRight :: Expression,
    _appImplicit :: IsImplicit
  }
  deriving stock (Eq, Show)

newtype Lambda = Lambda
  {_lambdaClauses :: NonEmpty LambdaClause}
  deriving stock (Eq, Show)

data LambdaClause = LambdaClause
  { _lambdaParameters :: NonEmpty PatternArg,
    _lambdaBody :: Expression
  }
  deriving stock (Eq, Show)

data FunctionParameter = FunctionParameter
  { _paramName :: Maybe VarName,
    _paramImplicit :: IsImplicit,
    _paramType :: Expression
  }
  deriving stock (Eq, Show)

newtype InductiveParameter = InductiveParameter
  { _inductiveParamName :: VarName
  }
  deriving stock (Eq, Show, Data)

data Function = Function
  { _funParameter :: FunctionParameter,
    _funReturn :: Expression
  }
  deriving stock (Eq, Show)

instance HasAtomicity Function where
  atomicity = const (Aggregate funFixity)

-- | Fully applied constructor in a pattern.
data ConstructorApp = ConstructorApp
  { _constrAppConstructor :: Name,
    _constrAppParameters :: [PatternArg],
    -- | ignore this field
    _constrAppType :: Maybe Expression
  }
  deriving stock (Eq, Show)

data PatternArg = PatternArg
  { _patternArgIsImplicit :: IsImplicit,
    _patternArgName :: Maybe VarName,
    _patternArgPattern :: Pattern
  }
  deriving stock (Eq, Show)

data Pattern
  = PatternVariable VarName
  | PatternConstructorApp ConstructorApp
  | PatternWildcard Wildcard
  deriving stock (Eq, Show)

data InductiveDef = InductiveDef
  { _inductiveName :: InductiveName,
    _inductiveBuiltin :: Maybe BuiltinInductive,
    _inductiveExamples :: [Example],
    _inductiveType :: Expression,
    _inductiveParameters :: [InductiveParameter],
    _inductiveConstructors :: [InductiveConstructorDef],
    _inductivePositive :: Bool,
    _inductivePragmas :: Pragmas
  }
  deriving stock (Eq, Show)

data InductiveConstructorDef = InductiveConstructorDef
  { _constructorName :: ConstrName,
    _constructorExamples :: [Example],
    _constructorType :: Expression,
    _constructorPragmas :: Pragmas
  }
  deriving stock (Eq, Show)

data AxiomDef = AxiomDef
  { _axiomName :: AxiomName,
    _axiomBuiltin :: Maybe BuiltinAxiom,
    _axiomType :: Expression,
    _axiomPragmas :: Pragmas
  }
  deriving stock (Eq, Show)

makeLenses ''Module
makeLenses ''InductiveParameter
makeLenses ''Case
makeLenses ''CaseBranch
makeLenses ''Let
makeLenses ''LetClause
makeLenses ''Example
makeLenses ''PatternArg
makeLenses ''FunctionParameter
makeLenses ''Function
makeLenses ''FunctionDef
makeLenses ''FunctionClause
makeLenses ''InductiveDef
makeLenses ''ModuleBody
makeLenses ''InductiveConstructorDef
makeLenses ''ConstructorApp
makeLenses ''AxiomDef

instance HasAtomicity Expression where
  atomicity = \case
    ExpressionIden {} -> Atom
    ExpressionHole {} -> Atom
    ExpressionUniverse u -> atomicity u
    ExpressionCase u -> atomicity u
    ExpressionLet l -> atomicity l
    ExpressionApplication a -> atomicity a
    ExpressionFunction f -> atomicity f
    ExpressionLiteral f -> atomicity f
    ExpressionLambda l -> atomicity l

instance HasAtomicity Case where
  atomicity = const Atom

instance HasAtomicity Application where
  atomicity = const (Aggregate appFixity)

instance HasAtomicity Let where
  atomicity Let {..} = atomicity _letExpression

instance HasAtomicity Lambda where
  atomicity = const Atom

instance HasAtomicity ConstructorApp where
  atomicity ConstructorApp {..}
    | null _constrAppParameters = Atom
    | otherwise = Aggregate appFixity

instance HasAtomicity Pattern where
  atomicity = \case
    PatternVariable {} -> Atom
    PatternConstructorApp a -> atomicity a
    PatternWildcard {} -> Atom

instance HasAtomicity PatternArg where
  atomicity p
    | Implicit <- p ^. patternArgIsImplicit = Atom
    | isJust (p ^. patternArgName) = Atom
    | otherwise = atomicity (p ^. patternArgPattern)

instance HasLoc InductiveConstructorDef where
  getLoc = getLoc . (^. constructorName)

instance HasLoc InductiveDef where
  getLoc = getLoc . (^. inductiveName)

instance HasLoc AxiomDef where
  getLoc = getLoc . (^. axiomName)

instance HasLoc FunctionDef where
  getLoc = getLoc . (^. funDefName)
