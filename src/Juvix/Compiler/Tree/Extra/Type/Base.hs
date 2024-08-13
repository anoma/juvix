module Juvix.Compiler.Tree.Extra.Type.Base where

import Juvix.Compiler.Tree.Language.Base
import Juvix.Compiler.Tree.Language.Type

mkTypeInteger :: Type
mkTypeInteger = TyInteger (TypeInteger Nothing Nothing)

mkTypeUInt8 :: Type
mkTypeUInt8 = TyInteger (TypeInteger (Just 0) (Just 255))

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
