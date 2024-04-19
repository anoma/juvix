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

registerEcPointDef :: (Member Builtins r) => InductiveDef -> Sem r ()
registerEcPointDef d = do
  unless (null (d ^. inductiveParameters)) (error "Ec.Point should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (error "Ec.Point should be in the small universe")
  registerBuiltin BuiltinEcPoint (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c] -> registerMkEcPoint c
    _ -> error "Ec.Point should have exactly one constructor"

registerMkEcPoint :: (Member Builtins r) => ConstructorDef -> Sem r ()
registerMkEcPoint d@ConstructorDef {..} = do
  let mkpt = _inductiveConstructorName
      ty = _inductiveConstructorType
  field_ <- getBuiltinName (getLoc d) BuiltinField
  pt <- getBuiltinName (getLoc d) BuiltinEcPoint
  unless (ty === (field_ --> field_ --> pt)) (error "EC.mkPoint has the wrong type")
  registerBuiltin BuiltinMkEcPoint mkpt

registerEcOp :: (Members '[Builtins, NameIdGen] r) => AxiomDef -> Sem r ()
registerEcOp f = do
  let ftype = f ^. axiomType
  pt <- getBuiltinName (getLoc f) BuiltinEcPoint
  field_ <- getBuiltinName (getLoc f) BuiltinField
  unless
    (ftype === (pt --> field_ --> pt --> pt))
    (error "ecOp must be of type Ec.Point -> Field -> Ec.Point -> Ec.Point")
  registerBuiltin BuiltinEcOp (f ^. axiomName)

registerRandomEcPoint :: (Members '[Builtins, NameIdGen] r) => AxiomDef -> Sem r ()
registerRandomEcPoint f = do
  let ftype = f ^. axiomType
  pt <- getBuiltinName (getLoc f) BuiltinEcPoint
  unless
    (ftype === pt)
    (error "randomEcPoint must be of type Ec.Point")
  registerBuiltin BuiltinRandomEcPoint (f ^. axiomName)
