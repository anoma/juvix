module Juvix.Compiler.Core.Language.Primitives where

{- This module defines JuvixCore primitive types. These do not necessarily
correspond directly to builtins, but may serve as primitive representations of
other types. For example, any type with two zero-argument constructors may be
represented by booleans, any type isomorphic to unary natural numbers may be
represented by integers with minimum value 0. -}

import Juvix.Compiler.Core.Language.Base

-- | Primitive type representation.
data Primitive
  = PrimInteger PrimIntegerInfo
  | PrimBool PrimBoolInfo
  | PrimString
  deriving stock (Eq)

-- | Info about a type represented as an integer.
data PrimIntegerInfo = PrimIntegerInfo
  { _infoMinValue :: Maybe Integer,
    _infoMaxValue :: Maybe Integer
  }
  deriving stock (Eq)

-- | Info about a type represented as a boolean.
data PrimBoolInfo = PrimBoolInfo
  { _infoTrueTag :: Tag,
    _infoFalseTag :: Tag
  }
  deriving stock (Eq)
