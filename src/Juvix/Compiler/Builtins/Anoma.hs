module Juvix.Compiler.Builtins.Anoma where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

registerAnomaGet :: (Members '[Builtins, NameIdGen] r) => AxiomDef -> Sem r ()
registerAnomaGet f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  keyT <- freshVar l "keyT"
  valueT <- freshVar l "valueT"
  let freeVars = HashSet.fromList [keyT, valueT]
  unless
    ((ftype ==% (u <>--> u <>--> keyT --> valueT)) freeVars)
    (error "anomaGet must be of type {Value Key : Type} -> Key -> Value")
  registerBuiltin BuiltinAnomaGet (f ^. axiomName)

registerAnomaEncode :: (Members '[Builtins, NameIdGen] r) => AxiomDef -> Sem r ()
registerAnomaEncode f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  encodeT <- freshVar l "encodeT"
  nat <- getBuiltinName (getLoc f) BuiltinNat
  let freeVars = HashSet.fromList [encodeT]
  unless
    ((ftype ==% (u <>--> encodeT --> nat)) freeVars)
    (error "anomaEncode must be of type {A : Type} -> A -> Nat")
  registerBuiltin BuiltinAnomaEncode (f ^. axiomName)

registerAnomaDecode :: (Members '[Builtins, NameIdGen] r) => AxiomDef -> Sem r ()
registerAnomaDecode f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  decodeT <- freshVar l "decodeT"
  nat <- getBuiltinName (getLoc f) BuiltinNat
  let freeVars = HashSet.fromList [decodeT]
  unless
    ((ftype ==% (u <>--> nat --> decodeT)) freeVars)
    (error "anomaEncode must be of type {A : Type} -> Nat -> A")
  registerBuiltin BuiltinAnomaDecode (f ^. axiomName)

registerAnomaVerifyDetached :: (Members '[Builtins, NameIdGen] r) => AxiomDef -> Sem r ()
registerAnomaVerifyDetached f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  decodeT <- freshVar l "signedDataT"
  byteArray <- getBuiltinName (getLoc f) BuiltinByteArray
  bool_ <- getBuiltinName (getLoc f) BuiltinBool
  let freeVars = HashSet.fromList [decodeT]
  unless
    ((ftype ==% (u <>--> byteArray --> decodeT --> byteArray --> bool_)) freeVars)
    (error "anomaVerifyDetached must be of type {A : Type} -> ByteArray -> A -> ByteArray -> Bool")
  registerBuiltin BuiltinAnomaVerifyDetached (f ^. axiomName)

registerAnomaSign :: (Members '[Builtins, NameIdGen] r) => AxiomDef -> Sem r ()
registerAnomaSign f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  dataT <- freshVar l "dataT"
  byteArray <- getBuiltinName (getLoc f) BuiltinByteArray
  let freeVars = HashSet.fromList [dataT]
  unless
    ((ftype ==% (u <>--> dataT --> byteArray --> byteArray)) freeVars)
    (error "anomaSign must be of type {A : Type} -> A -> ByteArray -> ByteArray")
  registerBuiltin BuiltinAnomaSign (f ^. axiomName)

registerAnomaVerifyWithMessage :: (Members '[Builtins, NameIdGen] r) => AxiomDef -> Sem r ()
registerAnomaVerifyWithMessage f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  dataT <- freshVar l "dataT"
  byteArray <- getBuiltinName (getLoc f) BuiltinByteArray
  maybe_ <- getBuiltinName (getLoc f) BuiltinMaybe
  let freeVars = HashSet.fromList [dataT]
  unless
    ((ftype ==% (u <>--> byteArray --> byteArray --> maybe_ @@ dataT)) freeVars)
    (error "anomaVerify must be of type {A : Type} -> byteArray -> byteArray -> Maybe A")
  registerBuiltin BuiltinAnomaVerifyWithMessage (f ^. axiomName)

registerAnomaSignDetached :: (Members '[Builtins, NameIdGen] r) => AxiomDef -> Sem r ()
registerAnomaSignDetached f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  dataT <- freshVar l "dataT"
  byteArray <- getBuiltinName (getLoc f) BuiltinByteArray
  let freeVars = HashSet.fromList [dataT]
  unless
    ((ftype ==% (u <>--> dataT --> byteArray --> byteArray)) freeVars)
    (error "anomaSignDetached must be of type {A : Type} -> A -> ByteArray -> ByteArray")
  registerBuiltin BuiltinAnomaSignDetached (f ^. axiomName)
