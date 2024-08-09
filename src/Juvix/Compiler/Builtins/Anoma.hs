module Juvix.Compiler.Builtins.Anoma where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkAnomaGet :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaGet f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  keyT <- freshVar l "keyT"
  valueT <- freshVar l "valueT"
  let freeVars = HashSet.fromList [keyT, valueT]
  unless ((ftype ==% (u <>--> u <>--> keyT --> valueT)) freeVars) $
    builtinsErrorText (getLoc f) "anomaGet must be of type {Value Key : Type} -> Key -> Value"

checkAnomaEncode :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaEncode f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  encodeT <- freshVar l "encodeT"
  nat <- getBuiltinName (getLoc f) BuiltinNat
  let freeVars = HashSet.fromList [encodeT]
  unless ((ftype ==% (u <>--> encodeT --> nat)) freeVars) $
    builtinsErrorText (getLoc f) "anomaEncode must be of type {A : Type} -> A -> Nat"

checkAnomaDecode :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaDecode f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  decodeT <- freshVar l "decodeT"
  nat <- getBuiltinName (getLoc f) BuiltinNat
  let freeVars = HashSet.fromList [decodeT]
  unless ((ftype ==% (u <>--> nat --> decodeT)) freeVars) $
    builtinsErrorText (getLoc f) "anomaEncode must be of type {A : Type} -> Nat -> A"

checkAnomaVerifyDetached :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaVerifyDetached f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  decodeT <- freshVar l "signedDataT"
  nat <- getBuiltinName (getLoc f) BuiltinNat
  bool_ <- getBuiltinName (getLoc f) BuiltinBool
  let freeVars = HashSet.fromList [decodeT]
  unless ((ftype ==% (u <>--> nat --> decodeT --> nat --> bool_)) freeVars) $
    builtinsErrorText (getLoc f) "anomaVerifyDetached must be of type {A : Type} -> Nat -> A -> Nat -> Bool"

checkAnomaSign :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaSign f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  dataT <- freshVar l "dataT"
  nat <- getBuiltinName (getLoc f) BuiltinNat
  let freeVars = HashSet.fromList [dataT]
  unless ((ftype ==% (u <>--> dataT --> nat --> nat)) freeVars) $
    builtinsErrorText (getLoc f) "anomaSign must be of type {A : Type} -> A -> Nat -> Nat"

checkAnomaVerifyWithMessage :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaVerifyWithMessage f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  dataT <- freshVar l "dataT"
  nat <- getBuiltinName (getLoc f) BuiltinNat
  maybe_ <- getBuiltinName (getLoc f) BuiltinMaybe
  let freeVars = HashSet.fromList [dataT]
  unless ((ftype ==% (u <>--> nat --> nat --> maybe_ @@ dataT)) freeVars) $
    builtinsErrorText (getLoc f) "anomaVerify must be of type {A : Type} -> Nat -> Nat -> Maybe A"

checkAnomaSignDetached :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaSignDetached f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  dataT <- freshVar l "dataT"
  nat <- getBuiltinName (getLoc f) BuiltinNat
  let freeVars = HashSet.fromList [dataT]
  unless ((ftype ==% (u <>--> dataT --> nat --> nat)) freeVars) $
    builtinsErrorText (getLoc f) "anomaSignDetached must be of type {A : Type} -> A -> Nat -> Nat"
