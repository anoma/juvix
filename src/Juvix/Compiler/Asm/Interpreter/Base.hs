module Juvix.Compiler.Asm.Interpreter.Base
  ( module Juvix.Compiler.Asm.Interpreter.Base,
    module Juvix.Compiler.Asm.Language,
  )
where

import Juvix.Compiler.Asm.Language

{-
  The following types of values may be stored in the heap or an activation
  frame.

  - Integer (arbitrary precision)
  - Boolean
  - String
  - Constructor data
  - Closure
-}

data Val
  = ValInteger Integer
  | ValBool Bool
  | ValString Text
  | ValUnit Unit
  | ValConstr Constr
  | ValClosure Closure
  deriving stock (Eq)

newtype Unit = Unit
  { _unitDisplay :: Bool
  }
  deriving stock (Eq)

data Constr = Constr
  { _constrTag :: Tag,
    _constrArgs :: [Val]
  }
  deriving stock (Eq)

data Closure = Closure
  { _closureSymbol :: Symbol,
    _closureArgs :: [Val]
  }
  deriving stock (Eq)

makeLenses ''Unit
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

instance HasAtomicity Val where
  atomicity = \case
    ValInteger {} -> Atom
    ValBool {} -> Atom
    ValString {} -> Atom
    ValUnit {} -> Atom
    ValConstr c -> atomicity c
    ValClosure cl -> atomicity cl
