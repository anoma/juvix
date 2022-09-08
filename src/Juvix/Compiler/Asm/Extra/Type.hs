module Juvix.Compiler.Asm.Extra.Type where

import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Language.Type

mkInteger :: Type
mkInteger = TyInteger (TypeInteger Nothing Nothing)

mkBool :: Type
mkBool = TyBool (TypeBool (BuiltinTag TagTrue) (BuiltinTag TagFalse))

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
  | _typeInductiveSymbol == _typeConstrInductive = x
unifyTypes y@TyConstr {} x@TyInductive {} = unifyTypes x y
unifyTypes (TyConstr c1) (TyConstr c2)
  | c1 ^. typeConstrInductive == c2 ^. typeConstrInductive
      && c1 ^. typeConstrTag == c2 ^. typeConstrTag =
      TyConstr (over typeConstrFields (zipWithExact unifyTypes (c2 ^. typeConstrFields)) c1)
unifyTypes (TyConstr c1) (TyConstr c2)
  | c1 ^. typeConstrInductive == c2 ^. typeConstrInductive =
      TyInductive (TypeInductive (c1 ^. typeConstrInductive))
unifyTypes (TyFun l1 r1) (TyFun l2 r2) = TyFun (unifyTypes l1 l2) (unifyTypes r1 r2)
unifyTypes (TyInteger (TypeInteger l1 u1)) (TyInteger (TypeInteger l2 u2)) =
  TyInteger (TypeInteger (unifyBounds min l1 l2) (unifyBounds max u1 u2))
  where
    unifyBounds :: (Integer -> Integer -> Integer) -> Maybe Integer -> Maybe Integer -> Maybe Integer
    unifyBounds _ Nothing _ = Nothing
    unifyBounds _ _ Nothing = Nothing
    unifyBounds f (Just x) (Just y) = Just (f x y)
unifyTypes x y | x == y = x
unifyTypes _ _ = error "not unifiable"

isSubtype :: Type -> Type -> Bool
isSubtype TyDynamic _ = True
isSubtype _ TyDynamic = True
isSubtype (TyConstr TypeConstr {..}) (TyInductive TypeInductive {..}) =
  _typeInductiveSymbol == _typeConstrInductive
isSubtype (TyConstr c1) (TyConstr c2) =
  c1 ^. typeConstrInductive == c2 ^. typeConstrInductive
    && c1 ^. typeConstrTag == c2 ^. typeConstrTag
    && all (uncurry isSubtype) (zip (c1 ^. typeConstrFields) (c2 ^. typeConstrFields))
isSubtype (TyFun l1 r1) (TyFun l2 r2) = isSubtype l2 l1 && isSubtype r1 r2
isSubtype (TyInteger (TypeInteger l1 u1)) (TyInteger (TypeInteger l2 u2)) =
  checkBounds (>=) l1 l2 && checkBounds (<=) u1 u2
  where
    checkBounds :: (Integer -> Integer -> Bool) -> Maybe Integer -> Maybe Integer -> Bool
    checkBounds _ Nothing Nothing = True
    checkBounds _ Nothing (Just _) = False
    checkBounds _ (Just _) Nothing = True
    checkBounds cmp (Just x) (Just y) = cmp x y
isSubtype x y = x == y
