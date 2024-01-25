module Juvix.Compiler.Tree.Language.Value where

import Juvix.Compiler.Tree.Language.Base

{-
  A value may be one of:

  - Integer (arbitrary precision)
  - Boolean
  - String
  - Constructor data
  - Closure
-}

data Value
  = ValInteger Integer
  | ValBool Bool
  | ValString Text
  | ValUnit
  | ValVoid
  | ValConstr Constr
  | ValClosure Closure
  deriving stock (Eq)

data Constr = Constr
  { _constrTag :: Tag,
    _constrArgs :: [Value]
  }
  deriving stock (Eq)

data Closure = Closure
  { _closureSymbol :: Symbol,
    _closureArgs :: [Value]
  }
  deriving stock (Eq)

makeLenses ''Constr
makeLenses ''Closure

instance HasAtomicity Constr where
  atomicity Constr {..}
    | null _constrArgs = Atom
    | otherwise = Aggregate appFixity

instance HasAtomicity Closure where
  atomicity Closure {..}
    | null _closureArgs = Atom
    | otherwise = Aggregate appFixity

instance HasAtomicity Value where
  atomicity = \case
    ValInteger {} -> Atom
    ValBool {} -> Atom
    ValString {} -> Atom
    ValUnit -> Atom
    ValVoid -> Atom
    ValConstr c -> atomicity c
    ValClosure cl -> atomicity cl
