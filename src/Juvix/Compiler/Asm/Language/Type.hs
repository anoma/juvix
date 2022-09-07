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
  deriving stock (Eq)

newtype TypeInductive = TypeInductive
  { _typeInductiveInductive :: Symbol
  }
  deriving stock (Eq)

data TypeConstr = TypeConstr
  { _typeConstrInductive :: Symbol,
    _typeConstrTag :: Tag,
    _typeConstrFields :: [Type]
  }
  deriving stock (Eq)

makeLenses ''TypeInteger
makeLenses ''TypeBool
makeLenses ''TypeInductive
makeLenses ''TypeConstr

unfoldType :: Type -> (Type, [Type])
unfoldType = \case
  TyFun l r ->
    let (tgt, args) = unfoldType r
     in (tgt, l : args)
  ty -> (ty, [])

typeArgs :: Type -> [Type]
typeArgs = snd . unfoldType

typeTarget :: Type -> Type
typeTarget = fst . unfoldType

unifyTypes :: Type -> Type -> Type
unifyTypes TyDynamic x = x
unifyTypes x TyDynamic = x
unifyTypes x@(TyInductive TypeInductive {..}) (TyConstr TypeConstr {..})
  | _typeInductiveInductive == _typeConstrInductive = x
unifyTypes y@TyConstr {} x@TyInductive {} = unifyTypes x y
unifyTypes (TyConstr c1) (TyConstr c2)
  | c1 ^. typeConstrInductive == c2 ^. typeConstrInductive
      && c1 ^. typeConstrTag /= c2 ^. typeConstrTag =
      TyInductive (TypeInductive (c1 ^. typeConstrInductive))
unifyTypes (TyFun l1 r1) (TyFun l2 r2) = TyFun (unifyTypes l1 l2) (unifyTypes r1 r2)
unifyTypes x y | x == y = x
unifyTypes _ _ = error "not unifiable"
