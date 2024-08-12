module Juvix.Compiler.Builtins.Anoma where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkAnomaGet :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaGet f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  keyT <- freshVar l "keyT"
  valueT <- freshVar l "valueT"
  let freeVars = hashSet [keyT, valueT]
  unless ((ftype ==% (u <>--> u <>--> keyT --> valueT)) freeVars) $
    builtinsErrorText (getLoc f) "anomaGet must be of type {Value Key : Type} -> Key -> Value"

checkAnomaEncode :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaEncode f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  encodeT <- freshVar l "encodeT"
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  let freeVars = hashSet [encodeT]
  unless ((ftype ==% (u <>--> encodeT --> nat)) freeVars) $
    builtinsErrorText (getLoc f) "anomaEncode must be of type {A : Type} -> A -> Nat"

checkAnomaDecode :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaDecode f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  decodeT <- freshVar l "decodeT"
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  let freeVars = HashSet.fromList [decodeT]
  unless ((ftype ==% (u <>--> nat --> decodeT)) freeVars) $
    builtinsErrorText (getLoc f) "anomaEncode must be of type {A : Type} -> Nat -> A"

checkAnomaVerifyDetached :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaVerifyDetached f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  decodeT <- freshVar l "signedDataT"
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  bool_ <- getBuiltinNameScoper (getLoc f) BuiltinBool
  let freeVars = HashSet.fromList [decodeT]
  unless ((ftype ==% (u <>--> nat --> decodeT --> nat --> bool_)) freeVars) $
    builtinsErrorText (getLoc f) "anomaVerifyDetached must be of type {A : Type} -> Nat -> A -> Nat -> Bool"

checkAnomaSign :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaSign f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  dataT <- freshVar l "dataT"
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  let freeVars = HashSet.fromList [dataT]
  unless ((ftype ==% (u <>--> dataT --> nat --> nat)) freeVars) $
    builtinsErrorText (getLoc f) "anomaSign must be of type {A : Type} -> A -> Nat -> Nat"

checkAnomaVerifyWithMessage :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaVerifyWithMessage f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  dataT <- freshVar l "dataT"
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  maybe_ <- getBuiltinNameScoper (getLoc f) BuiltinMaybe
  let freeVars = HashSet.fromList [dataT]
  unless ((ftype ==% (u <>--> nat --> nat --> maybe_ @@ dataT)) freeVars) $
    builtinsErrorText (getLoc f) "anomaVerify must be of type {A : Type} -> Nat -> Nat -> Maybe A"

checkAnomaSignDetached :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaSignDetached f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  dataT <- freshVar l "dataT"
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  let freeVars = HashSet.fromList [dataT]
  unless ((ftype ==% (u <>--> dataT --> nat --> nat)) freeVars) $
    builtinsErrorText (getLoc f) "anomaSignDetached must be of type {A : Type} -> A -> Nat -> Nat"
