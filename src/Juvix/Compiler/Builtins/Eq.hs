module Juvix.Compiler.Builtins.Eq where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude
import Juvix.Prelude.Pretty

checkEqDef :: forall r. (Members '[Reader BuiltinsTable, Error ScoperError] r) => InductiveDef -> Sem r ()
checkEqDef d = do
  let err :: forall a. Text -> Sem r a
      err = builtinsErrorText (getLoc d)
  let eqTxt = prettyText BuiltinEq
  unless (isSmallUniverse' (d ^. inductiveType)) (err (eqTxt <> " should be in the small universe"))
  case d ^. inductiveParameters of
    [_] -> return ()
    _ -> err (eqTxt <> " should have exactly one type parameter")
  case d ^. inductiveConstructors of
    [c1] -> checkMkEq c1
    _ -> err (eqTxt <> " should have exactly two constructors")

checkMkEq :: ConstructorDef -> Sem r ()
checkMkEq _ = return ()

checkIsEq :: FunctionDef -> Sem r ()
checkIsEq _ = return ()
