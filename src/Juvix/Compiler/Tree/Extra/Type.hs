module Juvix.Compiler.Tree.Extra.Type where

import Juvix.Compiler.Tree.Language.Base
import Juvix.Compiler.Tree.Language.Type

mkTypeInteger :: Type
mkTypeInteger = TyInteger (TypeInteger Nothing Nothing)

mkTypeBool :: Type
mkTypeBool = TyBool (TypeBool (BuiltinTag TagTrue) (BuiltinTag TagFalse))

mkTypeConstr :: Symbol -> Tag -> [Type] -> Type
mkTypeConstr ind tag argTypes = TyConstr (TypeConstr ind tag argTypes)

mkTypeInductive :: Symbol -> Type
mkTypeInductive ind = TyInductive (TypeInductive ind)

mkTypeFun :: [Type] -> Type -> Type
mkTypeFun args tgt = case args of
  [] -> tgt
  a : args' -> TyFun (TypeFun (a :| args') tgt)

unfoldType :: Type -> ([Type], Type)
unfoldType ty = (typeArgs ty, typeTarget ty)

-- | Converts e.g. `A -> B -> C -> D` to `(A, B, C) -> D` where `D` is an atom
uncurryType :: Type -> Type
uncurryType ty = case typeArgs ty of
  [] ->
    ty
  tyargs ->
    let ty' = uncurryType (typeTarget ty)
     in mkTypeFun (tyargs ++ typeArgs ty') (typeTarget ty')

-- | Converts e.g. `(A, B, C) -> (D, E) -> F` to `A -> B -> C -> D -> E -> F`
-- where `F` is an atom
curryType :: Type -> Type
curryType ty = case typeArgs ty of
  [] ->
    ty
  tyargs ->
    let ty' = curryType (typeTarget ty)
     in foldr (\tyarg ty'' -> mkTypeFun [tyarg] ty'') (typeTarget ty') tyargs

isSubtype :: Type -> Type -> Bool
isSubtype ty1 ty2 = case (ty1, ty2) of
  (TyDynamic, _) -> True
  (_, TyDynamic) -> True
  (TyConstr TypeConstr {..}, TyInductive TypeInductive {..}) ->
    _typeConstrInductive == _typeInductiveSymbol
  (TyConstr c1, TyConstr c2) ->
    c1 ^. typeConstrInductive == c2 ^. typeConstrInductive
      && c1 ^. typeConstrTag == c2 ^. typeConstrTag
      && all (uncurry isSubtype) (zip (c1 ^. typeConstrFields) (c2 ^. typeConstrFields))
  (TyFun t1, TyFun t2) ->
    let l1 = toList (t1 ^. typeFunArgs)
        l2 = toList (t2 ^. typeFunArgs)
        r1 = t1 ^. typeFunTarget
        r2 = t2 ^. typeFunTarget
     in length l1 == length l2 && all (uncurry isSubtype) (zip l2 l1) && isSubtype r1 r2
  (TyInteger (TypeInteger l1 u1), TyInteger (TypeInteger l2 u2)) ->
    checkBounds (>=) l1 l2 && checkBounds (<=) u1 u2
    where
      checkBounds :: (Integer -> Integer -> Bool) -> Maybe Integer -> Maybe Integer -> Bool
      checkBounds _ Nothing Nothing = True
      checkBounds _ Nothing (Just _) = False
      checkBounds _ (Just _) Nothing = True
      checkBounds cmp (Just x) (Just y) = cmp x y
  (TyBool {}, TyBool {}) -> True
  (TyString, TyString) -> True
  (TyUnit, TyUnit) -> True
  (TyVoid, TyVoid) -> True
  (TyInductive {}, TyInductive {}) -> ty1 == ty2
  (TyUnit, _) -> False
  (_, TyUnit) -> False
  (TyVoid, _) -> False
  (_, TyVoid) -> False
  (TyInteger {}, _) -> False
  (_, TyInteger {}) -> False
  (TyString, _) -> False
  (_, TyString) -> False
  (TyBool {}, _) -> False
  (_, TyBool {}) -> False
  (TyFun {}, _) -> False
  (_, TyFun {}) -> False
  (_, TyConstr {}) -> False

isSubtype' :: Type -> Type -> Bool
isSubtype' ty1 ty2
  -- The guard is to ensure correct behaviour with dynamic type targets. E.g.
  -- `A -> B -> C -> D` should be a subtype of `(A, B) -> *`.
  | tgt1 == TyDynamic || tgt2 == TyDynamic =
      isSubtype
        (curryType ty1)
        (curryType ty2)
  where
    tgt1 = typeTarget (uncurryType ty1)
    tgt2 = typeTarget (uncurryType ty2)
isSubtype' ty1 ty2 =
  isSubtype ty1 ty2
