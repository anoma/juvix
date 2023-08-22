-- | This file defines the tree representation of JuvixCore (Node datatype).
module Juvix.Compiler.Core.Language
  ( module Juvix.Compiler.Core.Language,
    module Juvix.Compiler.Core.Language.Nodes,
  )
where

import Juvix.Compiler.Core.Language.Nodes

{---------------------------------------------------------------------------------}

type Type = Node

type Var = Var' Info

type Ident = Ident' Info

type Constant = Constant' Info

type App = App' Info Node

type BuiltinApp = BuiltinApp' Info Node

type Constr = Constr' Info Node

type Binder = Binder' Node

type LambdaLhs = LambdaLhs' Info Type

type Lambda = Lambda' Info Node Type

type LetItem = LetItem' Node Type

type Let = Let' Info Node Type

type LetRec = LetRec' Info Node Type

type Case = Case' Info Info Node Type

type CaseBranch = CaseBranch' Info Node Type

type Match = Match' Info Node

type MatchBranch = MatchBranch' Info Node

type PatternWildcard = PatternWildcard' Info Node

type PatternConstr = PatternConstr' Info Node

type Pattern = Pattern' Info Node

type PiLhs = PiLhs' Info Node

type Pi = Pi' Info Node

type Univ = Univ' Info

type TypeConstr = TypeConstr' Info Node

type TypePrim = TypePrim' Info

type Dynamic = Dynamic' Info

type Bottom = Bottom' Info Node

{---------------------------------------------------------------------------------}

-- | `Node` is the type of nodes in the program tree. Extra
-- annotations are stored in the infos associated with each node.
data Node
  = NVar {-# UNPACK #-} !Var
  | NIdt {-# UNPACK #-} !Ident
  | NCst {-# UNPACK #-} !Constant
  | NApp {-# UNPACK #-} !App
  | NBlt {-# UNPACK #-} !BuiltinApp
  | NCtr {-# UNPACK #-} !Constr
  | NLam {-# UNPACK #-} !Lambda
  | NLet {-# UNPACK #-} !Let
  | NRec {-# UNPACK #-} !LetRec
  | NCase {-# UNPACK #-} !Case
  | NMatch {-# UNPACK #-} !Match
  | NPi {-# UNPACK #-} !Pi
  | NUniv {-# UNPACK #-} !Univ
  | NTyp {-# UNPACK #-} !TypeConstr
  | NPrim {-# UNPACK #-} !TypePrim
  | NDyn !Dynamic -- Dynamic is already a newtype, so it's unpacked.
  | NBot {-# UNPACK #-} !Bottom
  | -- Evaluation only: `Closure env node`.
    Closure
      { _closureEnv :: !Env,
        _closureNode :: !Node
      }
  deriving stock (Eq)

-- Other things we might need in the future:
-- - laziness annotations (converting these to closure/thunk creation should be
--   done further down the pipeline)

-- A node (term) is closed if it has no free variables, i.e., no de Bruijn
-- indices pointing outside the term.

-- Values are closed nodes of the following kinds:
-- - Constant
-- - Constr if all arguments are values
-- - Closure
--
-- Whether something is a value matters only for the evaluation semantics. It
-- doesn't matter much outside the evaluator. See also:
-- Juvix.Compiler.Core.Language.Value.

-- | All nodes in an environment must be values.
type Env = [Node]

instance HasAtomicity Node where
  atomicity = \case
    NVar x -> atomicity x
    NIdt x -> atomicity x
    NCst x -> atomicity x
    NApp x -> atomicity x
    NBlt x -> atomicity x
    NCtr x -> atomicity x
    NLam x -> atomicity x
    NLet x -> atomicity x
    NRec x -> atomicity x
    NCase x -> atomicity x
    NMatch x -> atomicity x
    NPi x -> atomicity x
    NUniv x -> atomicity x
    NTyp x -> atomicity x
    NPrim x -> atomicity x
    NDyn x -> atomicity x
    NBot x -> atomicity x
    Closure {} -> Aggregate lambdaFixity

emptyBinder :: Binder
emptyBinder =
  Binder
    { _binderName = "?",
      _binderLocation = Nothing,
      _binderType = NDyn (Dynamic mempty)
    }
