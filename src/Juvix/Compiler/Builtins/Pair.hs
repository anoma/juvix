module Juvix.Compiler.Builtins.Pair where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Prelude

registerPairDef :: (Member Builtins r) => InductiveDef -> Sem r ()
registerPairDef d = do
  unless (isSmallUniverse' (d ^. inductiveType)) (error "Pair should be in the small universe")
  registerBuiltin BuiltinPair (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c] -> registerPairConstr p1 p2 c
    _ -> error "Pair should have exactly one constructor"
  where
    (p1, p2) = case d ^. inductiveParameters of
      [v1, v2] -> (v1 ^. inductiveParamName, v2 ^. inductiveParamName)
      _ -> error "Pair should have exactly two type parameters"

registerPairConstr :: (Member Builtins r) => VarName -> VarName -> ConstructorDef -> Sem r ()
registerPairConstr a b d@ConstructorDef {..} = do
  let pr = _inductiveConstructorName
      ty = _inductiveConstructorType
  pair_ <- getBuiltinName (getLoc d) BuiltinPair
  let prty = a --> b --> pair_ @@ a @@ b
  unless (ty === prty) (error $ ", has the wrong type " <> ppTrace ty <> " | " <> ppTrace prty)
  registerBuiltin BuiltinPairConstr pr
