module Juvix.Compiler.Builtins.Maybe where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Prelude

checkMaybeDef :: forall r. (Members '[Reader BuiltinsTable, Error ScoperError] r) => InductiveDef -> Sem r ()
checkMaybeDef d = do
  let err :: forall a. Text -> Sem r a
      err = builtinsErrorText (getLoc d)
  unless (isSmallUniverse' (d ^. inductiveType)) (err "Maybe should be in the small universe")
  param :: VarName <- case d ^. inductiveParameters of
    [v] -> return (v ^. inductiveParamName)
    _ -> err "Maybe should have exactly one type parameter"
  case d ^. inductiveConstructors of
    [c1, c2] -> checkNothing param c1 >> checkJust param c2
    _ -> err "Maybe should have exactly two constructors"

checkNothing :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => VarName -> ConstructorDef -> Sem r ()
checkNothing a d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  maybe_ <- getBuiltinNameScoper (getLoc d) BuiltinMaybe
  let nothingty = maybe_ @@ a
  unless (ty === nothingty) $
    builtinsErrorMsg (getLoc d) $
      "nothing has the wrong type " <> ppOutDefault ty <> " | " <> ppOutDefault nothingty

checkJust :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => VarName -> ConstructorDef -> Sem r ()
checkJust a d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  maybe_ <- getBuiltinNameScoper (getLoc d) BuiltinMaybe
  let justty = a --> maybe_ @@ a
  unless (ty === justty) $
    builtinsErrorText (getLoc d) "just has the wrong type"
