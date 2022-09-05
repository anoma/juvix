module Juvix.Compiler.Core.Language
  ( module Juvix.Compiler.Core.Language,
    module Juvix.Compiler.Core.Language.Base,
    module Juvix.Compiler.Core.Language.Nodes,
  )
where

{-
  This file defines the tree representation of JuvixCore (Node datatype).
-}

import Juvix.Compiler.Core.Language.Base
import Juvix.Compiler.Core.Language.Nodes

{---------------------------------------------------------------------------------}
{- Program tree datatype -}

type instance FVar 'Core = Var' Info
type instance FIdent 'Core = Ident' Info
type instance FConstant 'Core = Constant' Info
type instance FApp 'Core = App' Info Node
type instance FBuiltinApp 'Core = BuiltinApp' Info Node
type instance FConstr 'Core = Constr' Info Node
type instance FLambda 'Core = Lambda' Info Node
type instance FLet 'Core = Let' Info Node
type instance FLetRec 'Core = LetRec' Info Node
type instance FCase 'Core = Case' Info Node
type instance FPi 'Core = Pi' Info Node
type instance FUniv 'Core = Univ' Info
type instance FTypeConstr 'Core = TypeConstr' Info Node
type instance FDynamic 'Core = Dynamic' Info

type Var = FVar 'Core
type Ident = FIdent 'Core
type Constant = FConstant 'Core
type App = FApp 'Core
type BuiltinApp = FBuiltinApp 'Core
type Constr = FConstr 'Core
type Lambda = FLambda 'Core
type Let = FLet 'Core
type LetRec = FLetRec 'Core
type Case = FCase 'Core
type Pi = FPi 'Core
type Univ = FUniv 'Core
type TypeConstr = FTypeConstr 'Core
type Dynamic = FDynamic 'Core

type CaseBranch = CaseBranch' Node

-- | `Node` is the type of nodes in the program tree. The nodes themselves
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
  | NRec {-# UNPACK #-} !LetRec
  | NCase {-# UNPACK #-} !Case
  | NPi {-# UNPACK #-} !Pi
  | NUniv {-# UNPACK #-} !Univ
  | NTyp {-# UNPACK #-} !TypeConstr
  | NDyn !Dynamic -- Dynamic is already a newtype, so it's unpacked.
  | -- Evaluation only: `Closure env body`.
    Closure
      { _closureEnv :: !Env,
        _closureLambda :: {-# UNPACK #-} !Lambda
      }
  deriving stock (Eq)

-- Other things we might need in the future:
-- - laziness annotations (converting these to closure/thunk creation should be
--   done further down the pipeline)

-- Other things we might need in the future:
-- - ConstFloat or ConstFixedPoint

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
    NPi x -> atomicity x
    NUniv x -> atomicity x
    NTyp x -> atomicity x
    NDyn x -> atomicity x
    Closure {} -> Aggregate lambdaFixity
