module Juvix.Compiler.Asm.Extra.Type where

import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Pretty

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

-- converts e.g. `A -> B -> C -> D` to `(A, B, C) -> D` where `D` is an atom
uncurryType :: Type -> Type
uncurryType ty = case typeArgs ty of
  [] ->
    ty
  tyargs ->
    let ty' = uncurryType (typeTarget ty)
     in mkTypeFun (tyargs ++ typeArgs ty') (typeTarget ty')

-- converts e.g. `(A, B, C) -> (D, E) -> F` to `A -> B -> C -> D -> E -> F`
-- where `F` is an atom
curryType :: Type -> Type
curryType ty = case typeArgs ty of
  [] ->
    ty
  tyargs ->
    let ty' = curryType (typeTarget ty)
     in foldr (\tyarg ty'' -> mkTypeFun [tyarg] ty'') (typeTarget ty') tyargs

unifyTypes :: forall r. Members '[Error AsmError, Reader (Maybe Location), Reader InfoTable] r => Type -> Type -> Sem r Type
unifyTypes ty1 ty2 = case (ty1, ty2) of
  (TyDynamic, x) -> return x
  (x, TyDynamic) -> return x
  (TyInductive TypeInductive {..}, TyConstr TypeConstr {..})
    | _typeInductiveSymbol == _typeConstrInductive ->
        return ty1
  (TyConstr {}, TyInductive {}) -> unifyTypes ty2 ty1
  (TyConstr c1, TyConstr c2)
    | c1 ^. typeConstrInductive == c2 ^. typeConstrInductive
        && c1 ^. typeConstrTag == c2 ^. typeConstrTag -> do
        flds <- zipWithM unifyTypes (c1 ^. typeConstrFields) (c2 ^. typeConstrFields)
        return $ TyConstr (set typeConstrFields flds c1)
  (TyConstr c1, TyConstr c2)
    | c1 ^. typeConstrInductive == c2 ^. typeConstrInductive ->
        return $ TyInductive (TypeInductive (c1 ^. typeConstrInductive))
  (TyFun t1, TyFun t2)
    | length (t1 ^. typeFunArgs) == length (t2 ^. typeFunArgs) -> do
        let args1 = toList (t1 ^. typeFunArgs)
            args2 = toList (t2 ^. typeFunArgs)
            tgt1 = t1 ^. typeFunTarget
            tgt2 = t2 ^. typeFunTarget
        args <- zipWithM unifyTypes args1 args2
        tgt <- unifyTypes tgt1 tgt2
        return $ TyFun (TypeFun (NonEmpty.fromList args) tgt)
  (TyInteger (TypeInteger l1 u1), TyInteger (TypeInteger l2 u2)) ->
    return $ TyInteger (TypeInteger (unifyBounds min l1 l2) (unifyBounds max u1 u2))
    where
      unifyBounds :: (Integer -> Integer -> Integer) -> Maybe Integer -> Maybe Integer -> Maybe Integer
      unifyBounds _ Nothing _ = Nothing
      unifyBounds _ _ Nothing = Nothing
      unifyBounds f (Just x) (Just y) = Just (f x y)
  (TyBool {}, TyBool {})
    | ty1 == ty2 -> return ty1
  (TyString, TyString) -> return TyString
  (TyUnit, TyUnit) -> return TyUnit
  (TyVoid, TyVoid) -> return TyVoid
  (TyInductive {}, TyInductive {})
    | ty1 == ty2 -> return ty1
  (TyUnit, _) -> err
  (_, TyUnit) -> err
  (TyVoid, _) -> err
  (_, TyVoid) -> err
  (TyInteger {}, _) -> err
  (_, TyInteger {}) -> err
  (TyString, _) -> err
  (_, TyString) -> err
  (TyBool {}, _) -> err
  (_, TyBool {}) -> err
  (TyFun {}, _) -> err
  (_, TyFun {}) -> err
  (TyInductive {}, _) -> err
  (_, TyConstr {}) -> err
  where
    err :: Sem r a
    err = do
      loc <- ask
      tab <- ask
      throw $ AsmError loc ("not unifiable: " <> ppTrace tab ty1 <> ", " <> ppTrace tab ty2)

unifyTypes' :: Member (Error AsmError) r => Maybe Location -> InfoTable -> Type -> Type -> Sem r Type
unifyTypes' loc tab ty1 ty2 =
  runReader loc $
    runReader tab $
    -- The `if` is to ensure correct behaviour with dynamic type targets. E.g.
    -- `(A, B) -> *` should unify with `A -> B -> C -> D`.
      if
          | tgt1 == TyDynamic && tgt2 == TyDynamic ->
              unifyTypes (curryType ty1) (curryType ty2)
          | tgt1 == TyDynamic ->
              unifyTypes (uncurryType ty1) ty2
          | tgt2 == TyDynamic ->
              unifyTypes ty1 (uncurryType ty2)
          | otherwise ->
              unifyTypes ty1 ty2
  where
    tgt1 = typeTarget (uncurryType ty1)
    tgt2 = typeTarget (uncurryType ty2)

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
