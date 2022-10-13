module Juvix.Compiler.Builtins.Boolean where

import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Abstract.Pretty
import Juvix.Compiler.Builtins.Effect
import Juvix.Prelude

registerBoolDef :: Member Builtins r => InductiveDef -> Sem r ()
registerBoolDef d = do
  unless (null (d ^. inductiveParameters)) (error "Bool should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (error "Bool should be in the small universe")
  registerBuiltin BuiltinBoolean (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c1, c2] -> registerTrue c1 >> registerFalse c2
    _ -> error "Bool should have exactly two constructors"

registerTrue :: Member Builtins r => InductiveConstructorDef -> Sem r ()
registerTrue d@InductiveConstructorDef {..} = do
  let ctorTrue = _constructorName
      ctorTy = _constructorType
  boolTy <- getBuiltinName (getLoc d) BuiltinBoolean
  unless (ctorTy === boolTy) (error $ "true has the wrong type " <> ppTrace ctorTy <> " | " <> ppTrace boolTy)
  registerBuiltin BuiltinBooleanTrue ctorTrue

registerFalse :: Member Builtins r => InductiveConstructorDef -> Sem r ()
registerFalse d@InductiveConstructorDef {..} = do
  let ctorFalse = _constructorName
      ctorTy = _constructorType
  boolTy <- getBuiltinName (getLoc d) BuiltinBoolean
  unless (ctorTy === boolTy) (error $ "false has the wrong type " <> ppTrace ctorTy <> " | " <> ppTrace boolTy)
  registerBuiltin BuiltinBooleanFalse ctorFalse
