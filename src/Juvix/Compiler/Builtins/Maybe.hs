module Juvix.Compiler.Builtins.Maybe where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Prelude

registerMaybeDef :: (Member Builtins r) => InductiveDef -> Sem r ()
registerMaybeDef d = do
  unless (isSmallUniverse' (d ^. inductiveType)) (error "Maybe should be in the small universe")
  registerBuiltin BuiltinMaybe (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c1, c2] -> registerNothing param c1 >> registerJust param c2
    _ -> error "Maybe should have exactly two constructors"
  where
    param :: VarName
    param = case d ^. inductiveParameters of
      [v] -> v ^. inductiveParamName
      _ -> error "Maybe should have exactly one type parameter"

registerNothing :: (Member Builtins r) => VarName -> ConstructorDef -> Sem r ()
registerNothing a d@ConstructorDef {..} = do
  let nothing = _inductiveConstructorName
      ty = _inductiveConstructorType
  maybe_ <- getBuiltinName (getLoc d) BuiltinMaybe
  let nothingty = maybe_ @@ a
  unless (ty === nothingty) (error $ "nothing has the wrong type " <> ppTrace ty <> " | " <> ppTrace nothingty)
  registerBuiltin BuiltinMaybeNothing nothing

registerJust :: (Member Builtins r) => VarName -> ConstructorDef -> Sem r ()
registerJust a d@ConstructorDef {..} = do
  let just_ = _inductiveConstructorName
      ty = _inductiveConstructorType
  maybe_ <- getBuiltinName (getLoc d) BuiltinMaybe
  let justty = a --> maybe_ @@ a
  unless (ty === justty) (error "just has the wrong type")
  registerBuiltin BuiltinMaybeJust just_
