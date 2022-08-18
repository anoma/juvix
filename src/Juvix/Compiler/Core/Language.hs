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

-- `Node` is the type of nodes in the program tree. The nodes themselves
-- contain only runtime-relevant information. Runtime-irrelevant annotations
-- (including all type information) are stored in the infos associated with each
-- node.
data Node
  = -- De Bruijn index of a locally bound variable.
    Var {varInfo :: !Info, varIndex :: !Index}
  | -- Global identifier of a function (with corresponding `Node` in the global
    -- context).
    Ident {identInfo :: !Info, identSymbol :: !Symbol}
  | Constant {constantInfo :: !Info, constantValue :: !ConstantValue}
  | -- An axiom. Computationally a unit.
    Axiom {axiomInfo :: !Info}
  | App {appInfo :: !Info, appLeft :: !Node, appRight :: !Node}
  | -- A builtin application. A builtin has no corresponding Node. It is treated
    -- specially by the evaluator and the code generator. For example, basic
    -- arithmetic operations go into `Builtin`. The number of arguments supplied
    -- must be equal to the number of arguments expected by the builtin
    -- operation (this simplifies evaluation and code generation). If you need
    -- partial application, eta-expand with lambdas, e.g., eta-expand `(+) 2` to
    -- `\x -> (+) 2 x`. See Transformation/Eta.hs.
    BuiltinApp {builtinInfo :: !Info, builtinOp :: !BuiltinOp, builtinArgs :: ![Node]}
  | -- A data constructor application. The number of arguments supplied must be
    -- equal to the number of arguments expected by the constructor.
    ConstrApp
      { constrInfo :: !Info,
        constrTag :: !Tag,
        constrArgs :: ![Node]
      }
  | Lambda {lambdaInfo :: !Info, lambdaBody :: !Node}
  | -- `let x := value in body` is not reducible to lambda + application for the purposes
    -- of ML-polymorphic / dependent type checking or code generation!
    Let {letInfo :: !Info, letValue :: !Node, letBody :: !Node}
  | -- One-level case matching on the tag of a data constructor: `Case value
    -- branches default`. `Case` is lazy: only the selected branch is evaluated.
    Case
      { caseInfo :: !Info,
        caseValue :: !Node,
        caseBranches :: ![CaseBranch],
        caseDefault :: !(Maybe Node)
      }
  | -- Lazy `if` on booleans. It is reasonable to separate booleans from general
    -- datatypes for the purposes of evaluation and code generation.
    If
      { ifInfo :: !Info,
        ifValue :: !Node,
        ifTrueBranch :: !Node,
        ifFalseBranch :: !Node
      }
  | -- Evaluation only: evaluated data constructor (the actual data). Arguments
    -- order: left to right. Arguments are values (see below).
    Data {dataInfo :: !Info, dataTag :: !Tag, dataArgs :: ![Node]}
  | -- Evaluation only: `Closure env body`
    Closure
      { closureInfo :: !Info,
        closureEnv :: !Env,
        closureBody :: !Node
      }
  | -- Evaluation only: a suspended term value which cannot be evaluated
    -- further, e.g., a hole applied to some arguments. `suspendedNode` must
    -- be closed (but need not be a value -- see below).
    Suspended {suspendedInfo :: !Info, suspendedNode :: !Node}

-- Other things we might need in the future:
-- - laziness annotations (converting these to closure/thunk creation should be
--   done further down the pipeline)
-- - with dependent types, it might actually be more reasonable to have Pi as
--   another node (because it's a binder); computationally it would be a unit,
--   erased in further stages of the pipeline
-- - with Pi a node, other basic type constructors should also be nodes:
--   TypeIdent (named type identifier available in the global context, e.g.,
--   inductive type), Universe

data ConstantValue
  = ConstInteger !Integer
  | ConstBool !Bool
  | ConstString !Text
  deriving stock (Eq)

-- Other things we might need in the future:
-- - ConstFloat

-- `CaseBranch tag argsNum branch`
-- - `argsNum` is the number of arguments of the constructor tagged with `tag`,
--   equal to the number of implicit binders above `branch`
data CaseBranch = CaseBranch {caseTag :: !Tag, caseBindersNum :: !Int, caseBranch :: !Node}
  deriving stock (Eq)

-- A node (term) is closed if it has no free variables, i.e., no de Bruijn
-- indices pointing outside the term.

-- Values are closed nodes of the following kinds:
-- - Constant
-- - Axiom
-- - Data
-- - Closure
-- - Suspended
--
-- Whether something is a value matters only for the evaluation semantics. It
-- doesn't matter much outside the evaluator.

-- All nodes in an environment must be values.
type Env = [Node]

instance HasAtomicity Node where
  atomicity = \case
    Var {} -> Atom
    Ident {} -> Atom
    Constant {} -> Atom
    Axiom {} -> Atom
    App {} -> Aggregate appFixity
    BuiltinApp {..} | null builtinArgs -> Atom
    BuiltinApp {} -> Aggregate lambdaFixity
    ConstrApp {..} | null constrArgs -> Atom
    ConstrApp {} -> Aggregate lambdaFixity
    Lambda {} -> Aggregate lambdaFixity
    Let {} -> Aggregate lambdaFixity
    Case {} -> Aggregate lambdaFixity
    If {} -> Aggregate lambdaFixity
    Data {} -> Aggregate lambdaFixity
    Closure {} -> Aggregate lambdaFixity
    Suspended {} -> Aggregate lambdaFixity

lambdaFixity :: Fixity
lambdaFixity = Fixity (PrecNat 0) (Unary AssocPostfix)

instance Eq Node where
  (==) :: Node -> Node -> Bool
  Var _ idx1 == Var _ idx2 = idx1 == idx2
  Ident _ sym1 == Ident _ sym2 = sym1 == sym2
  Constant _ v1 == Constant _ v2 = v1 == v2
  Axiom _ == Axiom _ = True
  App _ l1 r1 == App _ l2 r2 = l1 == l2 && r1 == r2
  BuiltinApp _ op1 args1 == BuiltinApp _ op2 args2 = op1 == op2 && args1 == args2
  ConstrApp _ tag1 args1 == ConstrApp _ tag2 args2 = tag1 == tag2 && args1 == args2
  Lambda _ b1 == Lambda _ b2 = b1 == b2
  Let _ v1 b1 == Let _ v2 b2 = v1 == v2 && b1 == b2
  Case _ v1 bs1 def1 == Case _ v2 bs2 def2 = v1 == v2 && bs1 == bs2 && def1 == def2
  If _ v1 tb1 fb1 == If _ v2 tb2 fb2 = v1 == v2 && tb1 == tb2 && fb1 == fb2
  Data _ tag1 args1 == Data _ tag2 args2 = tag1 == tag2 && args1 == args2
  Closure _ env1 b1 == Closure _ env2 b2 = env1 == env2 && b1 == b2
  Suspended _ b1 == Suspended _ b2 = b1 == b2
  _ == _ = False
