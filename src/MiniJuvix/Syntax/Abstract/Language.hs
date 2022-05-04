module MiniJuvix.Syntax.Abstract.Language
  ( module MiniJuvix.Syntax.Abstract.Language,
    module MiniJuvix.Syntax.Concrete.Language,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language (BackendItem, ForeignBlock (..), LiteralLoc (..), Usage)
import MiniJuvix.Syntax.Concrete.Name qualified as C
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S
import MiniJuvix.Syntax.Fixity
import MiniJuvix.Syntax.Universe

type TopModuleName = S.TopModulePath

type LocalModuleName = S.Symbol

type FunctionName = S.Symbol

type VarName = S.Symbol

type ConstrName = S.Symbol

type InductiveName = S.Symbol

type AxiomName = S.Symbol

-- TODO: Perhaps we could use a different Name type
--  that just includes fields (nameId + debug info)
-- requried in future passes.
type Name = S.Name

type TopModule = Module C.TopModulePath

type LocalModule = Module C.Symbol

data Module s = Module
  { _moduleName :: S.Name' s,
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
    _funDefTerminating :: Bool
  }
  deriving stock (Eq, Show)

data FunctionClause = FunctionClause
  { _clausePatterns :: [Pattern],
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
  --- | ExpressionMatch Match
  ---  ExpressionLambda Lambda not supported yet
  deriving stock (Eq, Show)

instance HasAtomicity Expression where
  atomicity e = case e of
    ExpressionIden {} -> Atom
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
    _appRight :: Expression
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
  | PatternWildcard
  | PatternEmpty
  deriving stock (Eq, Show)

data InductiveDef = InductiveDef
  { _inductiveName :: InductiveName,
    _inductiveParameters :: [FunctionParameter],
    _inductiveType :: Maybe Expression,
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

idenName :: Iden -> Name
idenName = \case
  IdenFunction n -> n ^. functionRefName
  IdenConstructor n -> n ^. constructorRefName
  IdenInductive n -> n ^. inductiveRefName
  IdenVar n -> S.unqualifiedSymbol n
  IdenAxiom n -> n ^. axiomRefName
