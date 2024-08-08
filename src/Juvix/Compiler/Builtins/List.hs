module Juvix.Compiler.Builtins.List where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Prelude

checkListDef :: forall r. (Members '[Builtins, Error BuiltinsError] r) => InductiveDef -> Sem r ()
checkListDef d = do
  let err :: forall a. Text -> Sem r a
      err = builtinsErrorText (getLoc d)
  unless (isSmallUniverse' (d ^. inductiveType)) (err "Lists should be in the small universe")
  param :: VarName <- case d ^. inductiveParameters of
    [v] -> return (v ^. inductiveParamName)
    _ -> err "List should have exactly one type parameter"

  case d ^. inductiveConstructors of
    [c1, c2] -> checkNil param c1 >> checkCons param c2
    _ -> err "List should have exactly two constructors"

checkNil :: (Members '[Builtins, Error BuiltinsError] r) => VarName -> ConstructorDef -> Sem r ()
checkNil a d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  list_ <- getBuiltinName (getLoc d) BuiltinList
  let nilty = list_ @@ a
  unless (ty === nilty) $
    builtinsErrorMsg (getLoc d) ("nil has the wrong type " <> ppOutDefault ty <> " | " <> ppOutDefault nilty)

checkCons :: (Members '[Builtins, Error BuiltinsError] r) => VarName -> ConstructorDef -> Sem r ()
checkCons a d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  list_ <- getBuiltinName (getLoc d) BuiltinList
  let consty = a --> list_ @@ a --> list_ @@ a
  unless (ty === consty) $
    builtinsErrorText (getLoc d) "cons has the wrong type"
