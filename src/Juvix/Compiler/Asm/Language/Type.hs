module Juvix.Compiler.Asm.Language.Type where

import Juvix.Compiler.Core.Language.Base

data Type
  = TyDynamic
  | TyInteger TypeInteger
  | TyBool TypeBool
  | TyString
  | TyInductive TypeInductive
  | -- | TyConstr represents the type of a specific constructor. It is a subtype
    -- of an appropriate TyInductive.
    TyConstr TypeConstr
  | TyFun Type Type
  deriving stock (Eq)

data TypeInteger = TypeInteger
  { _typeIntegerMinValue :: Maybe Integer,
    _typeIntegerMaxValue :: Maybe Integer
  }
  deriving stock (Eq)

data TypeBool = TypeBool
  { _typeBoolTrueTag :: Tag,
    _typeBoolFalseTag :: Tag
  }

instance Eq TypeBool where
  _ == _ = True

newtype TypeInductive = TypeInductive
  { _typeInductiveSymbol :: Symbol
  }
  deriving stock (Eq)

data TypeConstr = TypeConstr
  { _typeConstrInductive :: Symbol,
    _typeConstrTag :: Tag,
    _typeConstrFields :: [Type]
  }

instance Eq TypeConstr where
  (TypeConstr _ tag1 _) == (TypeConstr _ tag2 _) = tag1 == tag2

makeLenses ''TypeInteger
makeLenses ''TypeBool
makeLenses ''TypeInductive
makeLenses ''TypeConstr
