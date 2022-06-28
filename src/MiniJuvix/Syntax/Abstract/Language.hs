module MiniJuvix.Syntax.Abstract.Language
  ( module MiniJuvix.Syntax.Abstract.Language,
    module MiniJuvix.Syntax.Concrete.Language,
    module MiniJuvix.Syntax.Hole,
    module MiniJuvix.Syntax.Concrete.Builtins,
    module MiniJuvix.Syntax.Usage,
    module MiniJuvix.Syntax.Universe,
    module MiniJuvix.Syntax.Abstract.Name,
    module MiniJuvix.Syntax.Wildcard,
    module MiniJuvix.Syntax.IsImplicit,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Name
import MiniJuvix.Syntax.Concrete.Builtins
import MiniJuvix.Syntax.Concrete.Language (BackendItem, ForeignBlock (..), LiteralLoc (..), symbolLoc)
import MiniJuvix.Syntax.Hole
import MiniJuvix.Syntax.IsImplicit
import MiniJuvix.Syntax.Universe
import MiniJuvix.Syntax.Usage
import MiniJuvix.Syntax.Wildcard

type LocalModule = Module

type TopModule = Module

type TopModuleName = Name

data Module = Module
  { _moduleName :: Name,
    _moduleBody :: ModuleBody
  }
  deriving stock (Eq, Show)

newtype ModuleBody = ModuleBody
  { _moduleStatements :: [Statement]
  }
  deriving stock (Eq, Show)

data Statement
  = StatementInductive InductiveDef
  | StatementFunction FunctionDef
  | StatementImport TopModule
  | StatementForeign ForeignBlock
  | StatementLocalModule LocalModule
  | StatementAxiom AxiomDef
  deriving stock (Eq, Show)

data FunctionDef = FunctionDef
  { _funDefName :: FunctionName,
    _funDefTypeSig :: Expression,
    _funDefClauses :: NonEmpty FunctionClause,
    _funDefBuiltin :: Maybe BuiltinFunction,
    _funDefTerminating :: Bool
  }
  deriving stock (Eq, Show)

data FunctionClause = FunctionClause
  { _clauseName :: FunctionName,
    _clausePatterns :: [Pattern],
    _clauseBody :: Expression
  }
  deriving stock (Eq, Show)

newtype FunctionRef = FunctionRef
  {_functionRefName :: Name}
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

newtype ConstructorRef = ConstructorRef
  {_constructorRefName :: Name}
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

newtype InductiveRef = InductiveRef
  {_inductiveRefName :: Name}
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

newtype AxiomRef = AxiomRef
  {_axiomRefName :: Name}
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

data Iden
  = IdenFunction FunctionRef
  | IdenConstructor ConstructorRef
  | IdenVar VarName
  | IdenInductive InductiveRef
  | IdenAxiom AxiomRef
  deriving stock (Eq, Show)

data Expression
  = ExpressionIden Iden
  | ExpressionApplication Application
  | ExpressionUniverse Universe
  | ExpressionFunction Function
  | ExpressionLiteral LiteralLoc
  | ExpressionHole Hole
  --- | ExpressionMatch Match
  ---  ExpressionLambda Lambda not supported yet
  deriving stock (Eq, Show)

instance HasAtomicity Expression where
  atomicity = \case
    ExpressionIden {} -> Atom
    ExpressionHole {} -> Atom
    ExpressionUniverse u -> atomicity u
    ExpressionApplication a -> atomicity a
    ExpressionFunction f -> atomicity f
    ExpressionLiteral f -> atomicity f

data Match = Match
  { _matchExpression :: Expression,
    _matchAlts :: [MatchAlt]
  }
  deriving stock (Eq, Show)

data MatchAlt = MatchAlt
  { _matchAltPattern :: Pattern,
    _matchAltBody :: Expression
  }
  deriving stock (Eq, Show)

data Application = Application
  { _appLeft :: Expression,
    _appRight :: Expression,
    _appImplicit :: IsImplicit
  }
  deriving stock (Eq, Show)

instance HasAtomicity Application where
  atomicity = const (Aggregate appFixity)

newtype Lambda = Lambda
  {_lambdaClauses :: [LambdaClause]}
  deriving stock (Eq, Show)

data LambdaClause = LambdaClause
  { _lambdaParameters :: NonEmpty Pattern,
    _lambdaBody :: Expression
  }
  deriving stock (Eq, Show)

data FunctionParameter = FunctionParameter
  { _paramName :: Maybe VarName,
    _paramUsage :: Usage,
    _paramImplicit :: IsImplicit,
    _paramType :: Expression
  }
  deriving stock (Eq, Show)

data Function = Function
  { _funParameter :: FunctionParameter,
    _funReturn :: Expression
  }
  deriving stock (Eq, Show)

instance HasAtomicity Function where
  atomicity = const (Aggregate funFixity)

-- | Fully applied constructor in a pattern.
data ConstructorApp = ConstructorApp
  { _constrAppConstructor :: ConstructorRef,
    _constrAppParameters :: [Pattern]
  }
  deriving stock (Eq, Show)

data Pattern
  = PatternVariable VarName
  | PatternConstructorApp ConstructorApp
  | PatternWildcard Wildcard
  | PatternEmpty
  | PatternBraces Pattern
  deriving stock (Eq, Show)

data InductiveDef = InductiveDef
  { _inductiveName :: InductiveName,
    _inductiveBuiltin :: Maybe BuiltinInductive,
    _inductiveParameters :: [FunctionParameter],
    _inductiveType :: Expression,
    _inductiveConstructors :: [InductiveConstructorDef]
  }
  deriving stock (Eq, Show)

data InductiveConstructorDef = InductiveConstructorDef
  { _constructorName :: ConstrName,
    _constructorType :: Expression
  }
  deriving stock (Eq, Show)

data AxiomDef = AxiomDef
  { _axiomName :: AxiomName,
    _axiomBuiltin :: Maybe BuiltinAxiom,
    _axiomType :: Expression
  }
  deriving stock (Eq, Show)

makeLenses ''Module
makeLenses ''FunctionParameter
makeLenses ''Function
makeLenses ''FunctionDef
makeLenses ''FunctionClause
makeLenses ''InductiveDef
makeLenses ''ModuleBody
makeLenses ''InductiveConstructorDef
makeLenses ''ConstructorApp
makeLenses ''FunctionRef
makeLenses ''ConstructorRef
makeLenses ''InductiveRef
makeLenses ''AxiomRef
makeLenses ''AxiomDef

instance HasLoc InductiveConstructorDef where
  getLoc = getLoc . (^. constructorName)

instance HasLoc InductiveDef where
  getLoc = getLoc . (^. inductiveName)

instance HasLoc AxiomDef where
  getLoc = getLoc . (^. axiomName)

instance HasLoc FunctionDef where
  getLoc = getLoc . (^. funDefName)
