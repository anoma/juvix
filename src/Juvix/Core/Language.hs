{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Core.Language (
  module Juvix.Core.Language,
  module Juvix.Core.Language.Builtins,
  module Juvix.Core.Language.Base
) where

{-
  This file defines the tree representation of JuvixCore (Node datatype) and
  general recursors on it.
-}

import Juvix.Core.Language.Builtins
import Juvix.Core.Language.Base

{---------------------------------------------------------------------------------}
{- Program tree datatype -}

-- Consecutive symbol IDs for reachable user functions.
type Symbol = Word

-- Tag of a constructor, uniquely identifying it. Tag values are consecutive and
-- separate from symbol IDs. We might need fixed special tags in Core for common
-- "builtin" constructors, e.g., unit, nat, lists, pairs, so that the code
-- generator can treat them specially.
data Tag = BuiltinTag BuiltinDataTag | UserTag Word
  deriving stock (Eq)

-- de Bruijn index
type Index = Int

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
  | -- A builtin with no corresponding Node, treated specially by the evaluator
    -- and the code generator. For example, basic arithmetic operations go into
    -- `Builtin`.
    Builtin {builtinInfo :: !Info, builtinOp :: !BuiltinOp}
  | -- A data constructor (the function that creates the data).
    Constructor {constructorInfo :: !Info, constructorTag :: !Tag}
  | ConstValue {constantInfo :: !Info, constantValue :: !Constant}
  | -- An axiom. Computationally a unit.
    Axiom {axiomInfo :: !Info}
  | App {appInfo :: !Info, appLeft :: !Node, appRight :: !Node}
  | Lambda {lambdaInfo :: !Info, lambdaBody :: !Node}
  | -- `let x := value in body` is not reducible to lambda + application for the purposes
    -- of ML-polymorphic / dependent type checking or code generation!
    LetIn {letInfo :: !Info, letValue :: !Node, letBody :: !Node}
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
    -- order: right to left. Arguments are closed values.
    Data {dataInfo :: !Info, dataTag :: !Tag, dataArgs :: ![Node]}
  | -- Evaluation only: `LambdaClosure env body`
    LambdaClosure
      { closureInfo :: !Info,
        closureEnv :: !Env,
        closureBody :: !Node
      }
  | -- Evaluation only: a suspended term value which cannot be evaluated
    -- further, e.g., a hole applied to some arguments. The suspended term must
    -- be closed.
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

data Constant
  = ConstInteger !Integer
  | ConstBool !Bool

-- Other things we might need in the future:
-- - ConstString
-- - ConstFixedPoint

-- `CaseBranch tag argsNum branch`
-- - `argsNum` is the number of arguments of the constructor tagged with `tag`,
--   equal to the number of implicit binders above `branch`
data CaseBranch = CaseBranch {caseTag :: !Tag, caseBindersNum :: !Int, caseBranch :: !Node}

-- all nodes in an environment must be closed values (no free variables, i.e.,
-- no de Bruijn indices pointing outside the term)
type Env = [Node]
