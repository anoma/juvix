module Juvix.Compiler.Builtins.Cairo where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkPoseidonStateDef :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => InductiveDef -> Sem r ()
checkPoseidonStateDef d = do
  let err = builtinsErrorText (getLoc d)
  unless (null (d ^. inductiveParameters)) (err "PoseidonState should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (err "PoseidonState should be in the small universe")
  case d ^. inductiveConstructors of
    [c] -> checkMkPoseidonState c
    _ -> err "PoseidonState should have exactly one constructor"

checkMkPoseidonState :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => ConstructorDef -> Sem r ()
checkMkPoseidonState d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  field_ <- getBuiltinNameScoper (getLoc d) BuiltinField
  ps <- getBuiltinNameScoper (getLoc d) BuiltinPoseidonState
  unless (ty === (field_ --> field_ --> field_ --> ps)) $
    builtinsErrorText (getLoc d) "mkPoseidonState has the wrong type"

checkPoseidon :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkPoseidon f = do
  let ftype = f ^. axiomType
  ps <- getBuiltinNameScoper (getLoc f) BuiltinPoseidonState
  unless (ftype === (ps --> ps)) $
    builtinsErrorText (getLoc f) "poseidon must be of type PoseidonState -> PoseidonState"

checkEcPointDef :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => InductiveDef -> Sem r ()
checkEcPointDef d = do
  let err = builtinsErrorText (getLoc d)
  unless (null (d ^. inductiveParameters)) (err "Ec.Point should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (err "Ec.Point should be in the small universe")
  case d ^. inductiveConstructors of
    [c] -> checkMkEcPoint c
    _ -> err "Ec.Point should have exactly one constructor"

checkMkEcPoint :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => ConstructorDef -> Sem r ()
checkMkEcPoint d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  field_ <- getBuiltinNameScoper (getLoc d) BuiltinField
  pt <- getBuiltinNameScoper (getLoc d) BuiltinEcPoint
  unless (ty === (field_ --> field_ --> pt)) $
    builtinsErrorText (getLoc d) "EC.mkPoint has the wrong type"

checkEcOp :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkEcOp f = do
  let ftype = f ^. axiomType
  pt <- getBuiltinNameScoper (getLoc f) BuiltinEcPoint
  field_ <- getBuiltinNameScoper (getLoc f) BuiltinField
  unless (ftype === (pt --> field_ --> pt --> pt)) $
    builtinsErrorText (getLoc f) "ecOp must be of type Ec.Point -> Field -> Ec.Point -> Ec.Point"

checkRandomEcPoint :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkRandomEcPoint f = do
  let ftype = f ^. axiomType
  pt <- getBuiltinNameScoper (getLoc f) BuiltinEcPoint
  unless (ftype === pt) $
    builtinsErrorText (getLoc f) "randomEcPoint must be of type Ec.Point"

checkRangeCheck :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkRangeCheck f = do
  let ftype = f ^. funDefType
  field_ <- getBuiltinNameScoper (getLoc f) BuiltinField
  bool_ <- getBuiltinNameScoper (getLoc f) BuiltinBool
  unless (ftype === (field_ --> field_ --> bool_)) $
    builtinsErrorText (getLoc f) "rangeCheck must be of type Field -> Field -> Bool"
