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
