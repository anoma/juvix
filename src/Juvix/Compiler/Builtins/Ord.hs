module Juvix.Compiler.Builtins.Ord where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude
import Juvix.Prelude.Pretty

checkOrdDef :: forall r. (Members '[Reader BuiltinsTable, Error ScoperError] r) => InductiveDef -> Sem r ()
checkOrdDef d = do
  let err :: forall a. Text -> Sem r a
      err = builtinsErrorText (getLoc d)
  let eqTxt = prettyText BuiltinOrd
  unless (isSmallUniverse' (d ^. inductiveType)) (err (eqTxt <> " should be in the small universe"))
  case d ^. inductiveParameters of
    [_] -> return ()
    _ -> err (eqTxt <> " should have exactly one type parameter")
  case d ^. inductiveConstructors of
    [c1] -> checkMkOrd c1
    _ -> err (eqTxt <> " should have exactly one constructor")

checkMkOrd :: ConstructorDef -> Sem r ()
checkMkOrd _ = return ()

checkOrdCompare :: FunctionDef -> Sem r ()
checkOrdCompare _ = return ()
