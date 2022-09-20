module Juvix.Compiler.Asm.Extra.Type where

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
mkTypeFun = flip (foldr TyFun)

unfoldType :: Type -> ([Type], Type)
unfoldType = \case
  TyFun l r ->
    let (args, tgt) = unfoldType r
     in (l : args, tgt)
  ty -> ([], ty)

typeArgs :: Type -> [Type]
typeArgs = fst . unfoldType

typeTarget :: Type -> Type
typeTarget = snd . unfoldType

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
unifyTypes (TyFun l1 r1) (TyFun l2 r2) =
  TyFun <$> unifyTypes l1 l2 <*> unifyTypes r1 r2
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
isSubtype (TyFun l1 r1) (TyFun l2 r2) =
  isSubtype l2 l1 && isSubtype r1 r2
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
