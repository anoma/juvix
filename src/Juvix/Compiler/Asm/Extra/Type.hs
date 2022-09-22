module Juvix.Compiler.Asm.Extra.Type where

import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Language.Type
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

typeArgs :: Type -> [Type]
typeArgs = \case
  TyFun x -> toList (x ^. typeFunArgs)
  _ -> []

typeTarget :: Type -> Type
typeTarget ty = case ty of
  TyFun x -> x ^. typeFunTarget
  _ -> ty

unfoldType :: Type -> ([Type], Type)
unfoldType ty = (typeArgs ty, typeTarget ty)

uncurryType :: Type -> Type
uncurryType ty = case typeArgs ty of
  [] ->
    ty
  tyargs ->
    let ty' = uncurryType (typeTarget ty)
     in mkTypeFun (tyargs ++ typeArgs ty') (typeTarget ty')

unifyTypes :: Members '[Error AsmError, Reader (Maybe Location), Reader InfoTable] r => Type -> Type -> Sem r Type
unifyTypes TyDynamic x =
  return x
unifyTypes x TyDynamic =
  return x
unifyTypes x@(TyInductive TypeInductive {..}) (TyConstr TypeConstr {..})
  | _typeInductiveSymbol == _typeConstrInductive =
      return x
unifyTypes y@TyConstr {} x@TyInductive {} =
  unifyTypes x y
unifyTypes (TyConstr c1) (TyConstr c2)
  | c1 ^. typeConstrInductive == c2 ^. typeConstrInductive
      && c1 ^. typeConstrTag == c2 ^. typeConstrTag = do
      flds <- zipWithM unifyTypes (c1 ^. typeConstrFields) (c2 ^. typeConstrFields)
      return $ TyConstr (set typeConstrFields flds c1)
unifyTypes (TyConstr c1) (TyConstr c2)
  | c1 ^. typeConstrInductive == c2 ^. typeConstrInductive =
      return $ TyInductive (TypeInductive (c1 ^. typeConstrInductive))
unifyTypes (TyFun t1) (TyFun t2)
  | length (t1 ^. typeFunArgs) == length (t2 ^. typeFunArgs) = do
      let args1 = toList (t1 ^. typeFunArgs)
      let args2 = toList (t2 ^. typeFunArgs)
      let tgt1 = t1 ^. typeFunTarget
      let tgt2 = t2 ^. typeFunTarget
      args <- zipWithM unifyTypes args1 args2
      tgt <- unifyTypes tgt1 tgt2
      return $ TyFun (TypeFun (NonEmpty.fromList args) tgt)
unifyTypes (TyInteger (TypeInteger l1 u1)) (TyInteger (TypeInteger l2 u2)) =
  return $ TyInteger (TypeInteger (unifyBounds min l1 l2) (unifyBounds max u1 u2))
  where
    unifyBounds :: (Integer -> Integer -> Integer) -> Maybe Integer -> Maybe Integer -> Maybe Integer
    unifyBounds _ Nothing _ = Nothing
    unifyBounds _ _ Nothing = Nothing
    unifyBounds f (Just x) (Just y) = Just (f x y)
unifyTypes x y
  | x == y =
      return x
unifyTypes ty1 ty2 = do
  loc <- ask
  tab <- ask
  throw $ AsmError loc ("not unifiable: " `mappend` ppTrace tab ty1 `mappend` ", " `mappend` ppTrace tab ty2)

unifyTypes' :: Member (Error AsmError) r => Maybe Location -> InfoTable -> Type -> Type -> Sem r Type
unifyTypes' loc tab ty1 ty2 = runReader loc $ runReader tab $ unifyTypes ty1 ty2

isSubtype :: Type -> Type -> Bool
isSubtype TyDynamic _ =
  True
isSubtype _ TyDynamic =
  True
isSubtype (TyConstr TypeConstr {..}) (TyInductive TypeInductive {..}) =
  _typeConstrInductive == _typeInductiveSymbol
isSubtype (TyConstr c1) (TyConstr c2) =
  c1 ^. typeConstrInductive == c2 ^. typeConstrInductive
    && c1 ^. typeConstrTag == c2 ^. typeConstrTag
    && all (uncurry isSubtype) (zip (c1 ^. typeConstrFields) (c2 ^. typeConstrFields))
isSubtype (TyFun t1) (TyFun t2) =
  let l1 = toList (t1 ^. typeFunArgs)
      l2 = toList (t2 ^. typeFunArgs)
      r1 = t1 ^. typeFunTarget
      r2 = t2 ^. typeFunTarget
   in length l1 == length l2 && all (uncurry isSubtype) (zip l2 l1) && isSubtype r1 r2
isSubtype (TyInteger (TypeInteger l1 u1)) (TyInteger (TypeInteger l2 u2)) =
  checkBounds (>=) l1 l2 && checkBounds (<=) u1 u2
  where
    checkBounds :: (Integer -> Integer -> Bool) -> Maybe Integer -> Maybe Integer -> Bool
    checkBounds _ Nothing Nothing = True
    checkBounds _ Nothing (Just _) = False
    checkBounds _ (Just _) Nothing = True
    checkBounds cmp (Just x) (Just y) = cmp x y
isSubtype x y =
  x == y
