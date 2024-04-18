module Juvix.Compiler.Builtins.Cairo where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

registerPoseidonStateDef :: (Member Builtins r) => InductiveDef -> Sem r ()
registerPoseidonStateDef d = do
  unless (null (d ^. inductiveParameters)) (error "PoseidonState should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (error "PoseidonState should be in the small universe")
  registerBuiltin BuiltinPoseidonState (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c] -> registerMkPoseidonState c
    _ -> error "PoseidonState should have exactly one constructor"

registerMkPoseidonState :: (Member Builtins r) => ConstructorDef -> Sem r ()
registerMkPoseidonState d@ConstructorDef {..} = do
  let mkps = _inductiveConstructorName
      ty = _inductiveConstructorType
  field_ <- getBuiltinName (getLoc d) BuiltinField
  ps <- getBuiltinName (getLoc d) BuiltinPoseidonState
  unless (ty === (field_ --> field_ --> field_ --> ps)) (error "mkPoseidonState has the wrong type")
  registerBuiltin BuiltinMkPoseidonState mkps

registerPoseidon :: (Members '[Builtins, NameIdGen] r) => AxiomDef -> Sem r ()
registerPoseidon f = do
  let ftype = f ^. axiomType
  ps <- getBuiltinName (getLoc f) BuiltinPoseidonState
  unless
    (ftype === (ps --> ps))
    (error "poseidon must be of type PoseidonState -> PoseidonState")
  registerBuiltin BuiltinPoseidon (f ^. axiomName)
