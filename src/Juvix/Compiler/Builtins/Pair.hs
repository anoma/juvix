module Juvix.Compiler.Builtins.Pair where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Prelude

checkPairDef :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => InductiveDef -> Sem r ()
checkPairDef d = do
  unless (isSmallUniverse' (d ^. inductiveType)) (error "Pair should be in the small universe")
  (p1, p2) <- case d ^. inductiveParameters of
    [v1, v2] -> return (v1 ^. inductiveParamName, v2 ^. inductiveParamName)
    _ -> builtinsErrorText (getLoc d) "Pair should have exactly two type parameters"
  case d ^. inductiveConstructors of
    [c] -> checkPairConstr p1 p2 c
    _ -> builtinsErrorText (getLoc d) "Pair should have exactly one constructor"

checkPairConstr :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => VarName -> VarName -> ConstructorDef -> Sem r ()
checkPairConstr a b d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  pair_ <- getBuiltinNameScoper (getLoc d) BuiltinPair
  let prty = a --> b --> pair_ @@ a @@ b
  unless (ty === prty) $
    builtinsErrorMsg (getLoc d) $
      ", has the wrong type " <> ppOutDefault ty <> " | " <> ppOutDefault prty
