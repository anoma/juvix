{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Compiler.Core.Language
  ( module Juvix.Compiler.Core.Language,
    module Juvix.Compiler.Core.Language.Base,
  )
where

{-
  This file defines the tree representation of JuvixCore (Node datatype).
-}

import Juvix.Compiler.Core.Language.Base

{---------------------------------------------------------------------------------}
{- Program tree datatype -}

-- De Bruijn index of a locally bound variable.
data Var = Var {_varInfo :: !Info, _varIndex :: !Index}

-- Global identifier of a function (with corresponding `Node` in the global
-- context).
data Ident = Ident {_identInfo :: !Info, _identSymbol :: !Symbol}

data Constant = Constant {_constantInfo :: !Info, _constantValue :: !ConstantValue}

data App = App {_appInfo :: !Info, _appLeft :: !Node, _appRight :: !Node}

-- A builtin application. A builtin has no corresponding Node. It is treated
-- specially by the evaluator and the code generator. For example, basic
-- arithmetic operations go into `Builtin`. The number of arguments supplied
-- must be equal to the number of arguments expected by the builtin
-- operation (this simplifies evaluation and code generation). If you need
-- partial application, eta-expand with lambdas, e.g., eta-expand `(+) 2` to
-- `\x -> (+) 2 x`. See Transformation/Eta.hs.
data BuiltinApp = BuiltinApp
  { _builtinAppInfo :: !Info,
    _builtinAppOp :: !BuiltinOp,
    _builtinAppArgs :: ![Node]
  }

-- A data constructor application. The number of arguments supplied must be
-- equal to the number of arguments expected by the constructor.
data Constr = Constr
  { _constrInfo :: !Info,
    _constrTag :: !Tag,
    _constrArgs :: ![Node]
  }

data Lambda = Lambda {_lambdaInfo :: !Info, _lambdaBody :: !Node}

-- `let x := value in body` is not reducible to lambda + application for the purposes
-- of ML-polymorphic / dependent type checking or code generation!
data Let = Let {_letInfo :: !Info, _letValue :: !Node, _letBody :: !Node}

-- One-level case matching on the tag of a data constructor: `Case value
-- branches default`. `Case` is lazy: only the selected branch is evaluated.
data Case = Case
  { _caseInfo :: !Info,
    _caseValue :: !Node,
    _caseBranches :: ![CaseBranch],
    _caseDefault :: !(Maybe Node)
  }

-- Dependent Pi-type. Compilation-time only. Pi implicitly introduces a binder
-- in the body, exactly like Lambda. So `Pi info ty body` is `Pi x : ty .
-- body` in more familiar notation, but references to `x` in `body` are via de
-- Bruijn index. For example, Pi A : Type . A -> A translates to (omitting
-- Infos): Pi (Univ level) (Pi (Var 0) (Var 1)).
data Pi = Pi {_piInfo :: !Info, _piType :: !Type, _piBody :: !Type}

-- Universe. Compilation-time only.
data Univ = Univ {_univInfo :: !Info, _univLevel :: !Int}

-- Type constructor application. Compilation-time only.
data TypeConstr = TypeConstr
  { _typeConstrInfo :: !Info,
    _typeConstrSymbol :: !Symbol,
    _typeConstrArgs :: ![Type]
  }

-- `Node` is the type of nodes in the program tree. The nodes themselves
-- contain only runtime-relevant information. Runtime-irrelevant annotations
-- (including all type information) are stored in the infos associated with each
-- node.
data Node
  = NVar {-# UNPACK #-} !Var
  | NIdt {-# UNPACK #-} !Ident
  | NCst {-# UNPACK #-} !Constant
  | NApp {-# UNPACK #-} !App
  | NBlt {-# UNPACK #-} !BuiltinApp
  | NCtr {-# UNPACK #-} !Constr
  | NLam {-# UNPACK #-} !Lambda
  | NLet {-# UNPACK #-} !Let
  | NCase {-# UNPACK #-} !Case
  | NPi {-# UNPACK #-} !Pi
  | NUniv {-# UNPACK #-} !Univ
  | NTyp {-# UNPACK #-} !TypeConstr
  | -- Evaluation only: `Closure env body`
    Closure
      { _closureInfo :: !Info,
        _closureEnv :: !Env,
        _closureBody :: !Node
      }

-- Other things we might need in the future:
-- - laziness annotations (converting these to closure/thunk creation should be
--   done further down the pipeline)

data ConstantValue
  = ConstInteger !Integer
  | ConstString !Text
  deriving stock (Eq)

-- Other things we might need in the future:
-- - ConstFloat or ConstFixedPoint

-- `CaseBranch tag argsNum branch`
-- - `argsNum` is the number of arguments of the constructor tagged with `tag`,
--   equal to the number of implicit binders above `branch`
data CaseBranch = CaseBranch {_caseTag :: !Tag, _caseBindersNum :: !Int, _caseBranch :: !Node}
  deriving stock (Eq)

-- A node (term) is closed if it has no free variables, i.e., no de Bruijn
-- indices pointing outside the term.

-- Values are closed nodes of the following kinds:
-- - Constant
-- - Constr if all arguments are values
-- - Closure
--
-- Whether something is a value matters only for the evaluation semantics. It
-- doesn't matter much outside the evaluator.

-- All nodes in an environment must be values.
type Env = [Node]

type Type = Node

instance HasAtomicity Node where
  atomicity = \case
    NVar {} -> Atom
    NIdt {} -> Atom
    NCst {} -> Atom
    NApp {} -> Aggregate appFixity
    NBlt BuiltinApp {..} | null _builtinAppArgs -> Atom
    NBlt BuiltinApp {} -> Aggregate lambdaFixity
    NCtr Constr {..} | null _constrArgs -> Atom
    NCtr Constr {} -> Aggregate lambdaFixity
    NLam Lambda {} -> Aggregate lambdaFixity
    NLet Let {} -> Aggregate lambdaFixity
    NCase Case {} -> Aggregate lambdaFixity
    NPi Pi {} -> Aggregate lambdaFixity
    NUniv Univ {} -> Atom
    NTyp TypeConstr {} -> Aggregate appFixity
    Closure {} -> Aggregate lambdaFixity

lambdaFixity :: Fixity
lambdaFixity = Fixity (PrecNat 0) (Unary AssocPostfix)

instance Eq Node where
  (==) :: Node -> Node -> Bool
  NVar (Var _ idx1) == NVar (Var _ idx2) = idx1 == idx2
  NIdt (Ident _ sym1) == NIdt (Ident _ sym2) = sym1 == sym2
  NCst (Constant _ v1) == NCst (Constant _ v2) = v1 == v2
  NApp (App _ l1 r1) == NApp (App _ l2 r2) = l1 == l2 && r1 == r2
  NBlt (BuiltinApp _ op1 args1) == NBlt (BuiltinApp _ op2 args2) = op1 == op2 && args1 == args2
  NCtr (Constr _ tag1 args1) == NCtr (Constr _ tag2 args2) = tag1 == tag2 && args1 == args2
  NLam (Lambda _ b1) == NLam (Lambda _ b2) = b1 == b2
  NLet (Let _ v1 b1) == NLet (Let _ v2 b2) = v1 == v2 && b1 == b2
  NCase (Case _ v1 bs1 def1) == NCase (Case _ v2 bs2 def2) = v1 == v2 && bs1 == bs2 && def1 == def2
  NPi (Pi _ ty1 b1) == NPi (Pi _ ty2 b2) = ty1 == ty2 && b1 == b2
  NUniv (Univ _ l1) == NUniv (Univ _ l2) = l1 == l2
  NTyp (TypeConstr _ sym1 args1) == NTyp (TypeConstr _ sym2 args2) = sym1 == sym2 && args1 == args2
  Closure _ env1 b1 == Closure _ env2 b2 = env1 == env2 && b1 == b2
  _ == _ = False

makeLenses ''Var
makeLenses ''Ident
makeLenses ''Constant
makeLenses ''App
makeLenses ''BuiltinApp
makeLenses ''Constr
makeLenses ''Let
makeLenses ''Case
makeLenses ''Pi
makeLenses ''Univ
makeLenses ''TypeConstr
makeLenses ''CaseBranch
