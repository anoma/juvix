module Juvix.Compiler.Tree.Language.Value where

import Juvix.Compiler.Tree.Language.Base
import Juvix.Data.Field

{-
  A value may be one of:

  - Integer (arbitrary precision)
  - Field element
  - Boolean
  - String
  - Constructor data
  - Closure
-}

data Value
  = ValInteger Integer
  | ValField FField
  | ValBool Bool
  | ValString Text
  | ValUnit
  | ValVoid
  | ValConstr Constr
  | ValClosure Closure
  | ValUInt8 Word8
  | ValByteArray ByteString
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
    ValField {} -> Atom
    ValBool {} -> Atom
    ValString {} -> Atom
    ValUnit -> Atom
    ValVoid -> Atom
    ValConstr c -> atomicity c
    ValClosure cl -> atomicity cl
    ValUInt8 {} -> Atom
    ValByteArray {} -> Atom
