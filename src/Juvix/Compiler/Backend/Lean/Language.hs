module Juvix.Compiler.Backend.Lean.Language
  ( module Juvix.Compiler.Backend.Lean.Language,
    module Juvix.Compiler.Internal.Data.Name,
    module Juvix.Prelude,
  )
where

import Juvix.Compiler.Internal.Data.Name hiding (letFixity)
import Juvix.Prelude hiding (Cons, letFixity)

-- | Literals
data Literal
  = LitNumeric Integer
  | LitScientific Text 
  | LitString Text
  | LitChar Char
  | LitName Text
  deriving stock (Eq)

-- | Universe levels
data Level 
  = LevelZero
  | LevelSucc Level
  | LevelMax Level Level
  | LevelIMax Level Level
  | LevelParam Name
  | LevelMeta Int
  deriving stock (Eq)

-- | Types
data Type
  = TySort Level
  | TyVar TypeVar
  | TyFun FunType
  | TyPi PiType
  | TyApp TypeApp
  deriving stock (Eq)

data TypeVar = TypeVar
  { _typeVarName :: Name
  }
  deriving stock (Eq)

data FunType = FunType
  { _funTypeLeft :: Type,
    _funTypeRight :: Type
  }
  deriving stock (Eq)

data PiType = PiType
  { _piTypeBinder :: Binder,
    _piTypeBody :: Type
  }
  deriving stock (Eq)

data TypeApp = TypeApp
  { _typeAppHead :: Type,
    _typeAppArg :: Type
  }
  deriving stock (Eq)

-- | Binding information
data BinderInfo
  = BinderDefault
  | BinderImplicit
  | BinderStrictImplicit
  | BinderInstImplicit
  deriving stock (Eq)

-- | Binders
data Binder = Binder
  { _binderName :: Name,
    _binderType :: Maybe Type,
    _binderInfo :: BinderInfo,
    _binderDefault :: Maybe Expression
  }
  deriving stock (Eq)

-- | Patterns
data Pattern
  = PatLit (WithLoc Literal)
  | PatVar Name
  | PatCtor Name [Pattern]
  | PatInaccessible Expression
  | PatTyped Pattern Type 
  | PatAs Name Pattern
  deriving stock (Eq)

-- | Expressions
data Expression
  = ExprVar Int
  | ExprSort Level
  | ExprConst Name [Level]
  | ExprApp Application
  | ExprLambda Lambda
  | ExprPi PiType
  | ExprLet Let
  | ExprLiteral (WithLoc Literal)
  | ExprMatch Case
  | ExprHole Interval
  | ExprMeta Name
  | ExprQuote Expression
  | ExprStruct Structure
  | ExprArray [Expression]
  | ExprProj Projection
  | ExprIf If
  deriving stock (Eq)

data Application = Application
  { _appLeft :: Expression,
    _appRight :: Expression
  }
  deriving stock (Eq)

data Lambda = Lambda
  { _lambdaBinder :: Binder,
    _lambdaBody :: Expression
  }
  deriving stock (Eq)

data Let = Let
  { _letName :: Name,
    _letType :: Maybe Type,
    _letValue :: Expression,
    _letBody :: Expression
  }
  deriving stock (Eq)

data Case = Case
  { _caseValue :: Expression,
    _caseBranches :: NonEmpty CaseBranch
  }
  deriving stock (Eq)

data CaseBranch = CaseBranch
  { _caseBranchPattern :: Pattern,
    _caseBranchBody :: Expression
  }
  deriving stock (Eq)

data Structure = Structure
  { _structBase :: Maybe Expression,
    _structFields :: [(Name, Expression)]
  }
  deriving stock (Eq)

data Projection = Projection
  { _projExpr :: Expression,
    _projField :: Name
  }
  deriving stock (Eq)

data If = If
  { _ifCond :: Expression,
    _ifThen :: Expression,
    _ifElse :: Expression
  }
  deriving stock (Eq)

-- | Commands
data Command
  = CmdDefinition Definition
  | CmdAxiom Axiom
  | CmdInductive Inductive
  | CmdStructure Structure'
  | CmdClass Class
  | CmdInstance Instance
  deriving stock (Eq)

data Definition = Definition
  { _definitionName :: Name,
    _definitionType :: Maybe Type,
    _definitionBody :: Expression,
    _definitionAttrs :: [Attribute]
  }
  deriving stock (Eq)

data Axiom = Axiom
  { _axiomName :: Name,
    _axiomType :: Type,
    _axiomAttrs :: [Attribute]
  }
  deriving stock (Eq)

data Inductive = Inductive
  { _inductiveName :: Name,
    _inductiveParams :: [Binder],
    _inductiveType :: Type,
    _inductiveCtors :: [(Name, Type)]
  }
  deriving stock (Eq)

data Structure' = Structure'
  { _structureName :: Name,
    _structureParams :: [Binder],
    _structureFields :: [(Name, Type)],
    _structureExtends :: [Type]
  }
  deriving stock (Eq)

data Class = Class
  { _className :: Name,
    _classParams :: [Binder],
    _classFields :: [(Name, Type)]
  }
  deriving stock (Eq)

data Instance = Instance
  { _instanceName :: Maybe Name,
    _instanceType :: Type,
    _instanceValue :: Expression,
    _instancePriority :: Maybe Int
  }
  deriving stock (Eq)

-- | Module-level commands
data ModuleCommand
  = ModImport Name
  | ModOpen OpenNamespace
  | ModNamespace Name [Command]
  | ModSection Name [Command]
  | ModSetOption Name Text
  | ModCommand Command
  deriving stock (Eq)

data OpenNamespace = OpenNamespace
  { _openNamespace :: Name,
    _openOption :: OpenOption
  }
  deriving stock (Eq)

data OpenOption
  = OpenAll
  | OpenOnly [Name]
  | OpenHiding [Name]
  | OpenRenaming [(Name, Name)]
  deriving stock (Eq)

data Attribute = Attribute
  { _attrName :: Name,
    _attrArgs :: [Expression],
    _attrLocal :: Bool,
    _attrScoped :: Bool
  }
  deriving stock (Eq)

data Module = Module
  { _modulePrelude :: Bool,
    _moduleImports :: [Name],
    _moduleCommands :: [ModuleCommand]
  }
  deriving stock (Eq)

-- Generate lenses
makeLenses ''TypeVar
makeLenses ''FunType
makeLenses ''PiType
makeLenses ''TypeApp
makeLenses ''Binder
makeLenses ''Application
makeLenses ''Lambda
makeLenses ''Let
makeLenses ''Case
makeLenses ''CaseBranch
makeLenses ''Structure
makeLenses ''Projection
makeLenses ''If
makeLenses ''Definition
makeLenses ''Axiom
makeLenses ''Inductive
makeLenses ''Structure'
makeLenses ''Class
makeLenses ''Instance
makeLenses ''OpenNamespace
makeLenses ''Attribute
makeLenses ''Module

piFixity :: Fixity
piFixity =
  Fixity
    { _fixityPrecedence = PrecNat 4,
      _fixityArity = OpBinary AssocRight,
      _fixityId = Nothing
    }

lambdaFixity :: Fixity
lambdaFixity =
  Fixity
    { _fixityPrecedence = PrecNat 3,
      _fixityArity = OpUnary AssocPostfix,
      _fixityId = Nothing
    }

caseFixity :: Fixity
caseFixity =
  Fixity
    { _fixityPrecedence = PrecNat 2,
      _fixityArity = OpBinary AssocRight,
      _fixityId = Nothing
    }

letFixity :: Fixity
letFixity =
  Fixity
    { _fixityPrecedence = PrecNat 1,
      _fixityArity = OpBinary AssocRight,
      _fixityId = Nothing
    }

consFixity :: Fixity
consFixity =
  Fixity
    { _fixityPrecedence = PrecNat 8,
      _fixityArity = OpBinary AssocRight,
      _fixityId = Nothing
    }

andFixity :: Fixity
andFixity =
  Fixity
    { _fixityPrecedence = PrecNat 7,
      _fixityArity = OpBinary AssocRight,
      _fixityId = Nothing
    }

orFixity :: Fixity
orFixity =
  Fixity
    { _fixityPrecedence = PrecNat 6,
      _fixityArity = OpBinary AssocRight,
      _fixityId = Nothing
    }

ifFixity :: Fixity
ifFixity =
  Fixity
    { _fixityPrecedence = PrecNat 1,
      _fixityArity = OpBinary AssocRight,
      _fixityId = Nothing
    }

instance HasAtomicity Literal where
  atomicity _ = Atom

instance HasAtomicity Level where
  atomicity = \case
    LevelZero -> Atom
    LevelSucc {} -> Aggregate appFixity
    LevelMax {} -> Aggregate appFixity
    LevelIMax {} -> Aggregate appFixity
    LevelParam {} -> Atom
    LevelMeta {} -> Atom

instance HasAtomicity Type where
  atomicity = \case
    TySort {} -> Atom
    TyVar {} -> Atom
    TyFun {} -> Aggregate funFixity
    TyPi {} -> Aggregate piFixity
    TyApp {} -> Aggregate appFixity

instance HasAtomicity Expression where
  atomicity = \case
    ExprVar {} -> Atom
    ExprSort {} -> Atom
    ExprConst {} -> Atom
    ExprApp {} -> Aggregate appFixity
    ExprLambda {} -> Aggregate lambdaFixity
    ExprPi {} -> Aggregate piFixity
    ExprLet {} -> Aggregate letFixity
    ExprLiteral {} -> Atom
    ExprMatch {} -> Aggregate caseFixity
    ExprHole {} -> Atom
    ExprMeta {} -> Atom
    ExprQuote {} -> Atom
    ExprStruct {} -> Aggregate appFixity
    ExprArray {} -> Aggregate appFixity
    ExprProj {} -> Aggregate appFixity
    ExprIf {} -> Aggregate ifFixity

instance HasAtomicity Pattern where
  atomicity = \case
    PatLit {} -> Atom
    PatVar {} -> Atom
    PatCtor _ args
      | null args -> Atom
      | otherwise -> Aggregate appFixity
    PatInaccessible {} -> Atom
    PatTyped {} -> Aggregate appFixity
    PatAs {} -> Aggregate appFixity

instance HasAtomicity Binder where
  atomicity _ = Atom

defaultInterval :: Interval
defaultInterval =
  Interval
    { _intervalFile = fromJust (parseAbsFile "/dev/null"),
      _intervalStart = mkInitialFileLoc,
      _intervalEnd = mkInitialFileLoc
    }

instance HasLoc Literal where
  getLoc = \case
    LitNumeric {} -> defaultInterval
    LitScientific {} -> defaultInterval
    LitString {} -> defaultInterval
    LitChar {} -> defaultInterval
    LitName {} -> defaultInterval

instance HasLoc Level where
  getLoc = const defaultInterval

instance HasLoc TypeVar where
  getLoc TypeVar {..} = getLoc _typeVarName

instance HasLoc Type where
  getLoc = \case
    TySort _ -> defaultInterval
    TyVar v -> getLoc v
    TyFun FunType {..} -> getLoc _funTypeLeft <> getLoc _funTypeRight
    TyPi PiType {..} -> getLoc _piTypeBinder <> getLoc _piTypeBody
    TyApp TypeApp {..} -> getLoc _typeAppHead <> getLoc _typeAppArg

instance HasLoc Expression where
  getLoc = \case
    ExprVar {} -> defaultInterval
    ExprSort _ -> defaultInterval
    ExprConst name _ -> getLoc name
    ExprApp Application {..} -> getLoc _appLeft <> getLoc _appRight
    ExprLambda Lambda {..} -> getLoc _lambdaBinder <> getLoc _lambdaBody
    ExprPi PiType {..} -> getLoc _piTypeBinder <> getLoc _piTypeBody
    ExprLet Let {..} -> getLoc _letName <> getLoc _letBody
    ExprLiteral x -> x ^. withLocInt
    ExprMatch Case {..} -> getLoc _caseValue <> getLocSpan _caseBranches
    ExprHole loc -> loc
    ExprMeta name -> getLoc name
    ExprQuote expr -> getLoc expr
    ExprStruct Structure {..} ->
      maybe defaultInterval getLoc _structBase
        <> case nonEmpty (map snd _structFields) of
          Just neFields -> getLocSpan neFields
          Nothing -> defaultInterval
    ExprArray elems ->
      case nonEmpty elems of
        Just neElems -> getLocSpan neElems
        Nothing -> defaultInterval
    ExprProj Projection {..} -> getLoc _projExpr <> getLoc _projField
    ExprIf If {..} -> getLoc _ifCond <> getLoc _ifThen <> getLoc _ifElse

instance HasLoc Pattern where
  getLoc = \case
    PatLit x -> x ^. withLocInt
    PatVar name -> getLoc name
    PatCtor name args ->
      getLoc name
        <> case nonEmpty args of
          Just neArgs -> getLocSpan neArgs
          Nothing -> defaultInterval
    PatInaccessible expr -> getLoc expr
    PatTyped pat ty -> getLoc pat <> getLoc ty
    PatAs name pat -> getLoc name <> getLoc pat

instance HasLoc Binder where
  getLoc Binder {..} =
    getLoc _binderName <> maybe defaultInterval getLoc _binderType <> maybe defaultInterval getLoc _binderDefault

instance HasLoc Application where
  getLoc Application {..} = getLoc _appLeft <> getLoc _appRight

instance HasLoc Case where
  getLoc Case {..} = getLoc _caseValue <> getLocSpan _caseBranches

instance HasLoc CaseBranch where
  getLoc CaseBranch {..} = getLoc _caseBranchPattern <> getLoc _caseBranchBody

instance HasLoc Let where
  getLoc Let {..} = getLoc _letName <> getLoc _letBody

instance HasLoc Lambda where
  getLoc Lambda {..} = getLoc _lambdaBinder <> getLoc _lambdaBody

instance HasLoc If where
  getLoc If {..} = getLoc _ifCond <> getLoc _ifThen <> getLoc _ifElse

instance HasLoc Structure where
  getLoc Structure {..} =
    maybe defaultInterval getLoc _structBase
      <> case nonEmpty (map snd _structFields) of
        Just neFields -> getLocSpan neFields
        Nothing -> defaultInterval

instance HasLoc Projection where
  getLoc Projection {..} = getLoc _projExpr <> getLoc _projField
