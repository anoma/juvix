module Juvix.Syntax.Abstract.Language
  ( module Juvix.Syntax.Abstract.Language,
    module Juvix.Syntax.Concrete.Language,
    module Juvix.Syntax.Hole,
    module Juvix.Syntax.Concrete.Builtins,
    module Juvix.Syntax.Concrete.Literal,
    module Juvix.Syntax.Usage,
    module Juvix.Syntax.Universe,
    module Juvix.Syntax.Abstract.Name,
    module Juvix.Syntax.Wildcard,
    module Juvix.Syntax.IsImplicit,
  )
where

import Juvix.Prelude
import Juvix.Syntax.Abstract.Name
import Juvix.Syntax.Concrete.Builtins
import Juvix.Syntax.Concrete.Language (BackendItem, ForeignBlock (..), symbolLoc)
import Juvix.Syntax.Concrete.Literal
import Juvix.Syntax.Hole
import Juvix.Syntax.IsImplicit
import Juvix.Syntax.Universe
import Juvix.Syntax.Usage
import Juvix.Syntax.Wildcard

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
    _clausePatterns :: [PatternArg],
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
    _constrAppParameters :: [PatternArg]
  }
  deriving stock (Eq, Show)

data PatternArg = PatternArg
  { _patternArgIsImplicit :: IsImplicit,
    _patternArgPattern :: Pattern
  }
  deriving stock (Eq, Show)

data Pattern
  = PatternVariable VarName
  | PatternConstructorApp ConstructorApp
  | PatternWildcard Wildcard
  | PatternEmpty
  deriving stock (Eq, Show)

data InductiveDef = InductiveDef
  { _inductiveName :: InductiveName,
    _inductiveBuiltin :: Maybe BuiltinInductive,
    _inductiveParameters :: [FunctionParameter],
    _inductiveType :: Expression,
    _inductiveConstructors :: [InductiveConstructorDef],
    _inductiveNoPositivity :: Bool
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
makeLenses ''PatternArg
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
