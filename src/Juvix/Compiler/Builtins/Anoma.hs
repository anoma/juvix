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
  byteArray <- getBuiltinNameScoper (getLoc f) BuiltinByteArray
  bool_ <- getBuiltinNameScoper (getLoc f) BuiltinBool
  let freeVars = HashSet.fromList [decodeT]
  unless
    ((ftype ==% (u <>--> byteArray --> decodeT --> byteArray --> bool_)) freeVars)
    $ builtinsErrorText (getLoc f) "anomaVerifyDetached must be of type {A : Type} -> ByteArray -> A -> ByteArray -> Bool"

checkAnomaSign :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaSign f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  dataT <- freshVar l "dataT"
  byteArray <- getBuiltinNameScoper (getLoc f) BuiltinByteArray
  let freeVars = HashSet.fromList [dataT]
  unless
    ((ftype ==% (u <>--> dataT --> byteArray --> byteArray)) freeVars)
    $ builtinsErrorText (getLoc f) "anomaSign must be of type {A : Type} -> A -> ByteArray -> ByteArray"

checkAnomaVerifyWithMessage :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaVerifyWithMessage f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  dataT <- freshVar l "dataT"
  byteArray <- getBuiltinNameScoper (getLoc f) BuiltinByteArray
  maybe_ <- getBuiltinNameScoper (getLoc f) BuiltinMaybe
  let freeVars = HashSet.fromList [dataT]
  unless
    ((ftype ==% (u <>--> byteArray --> byteArray --> maybe_ @@ dataT)) freeVars)
    $ builtinsErrorText (getLoc f) "anomaVerify must be of type {A : Type} -> byteArray -> byteArray -> Maybe A"

checkAnomaSignDetached :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaSignDetached f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  dataT <- freshVar l "dataT"
  byteArray <- getBuiltinNameScoper (getLoc f) BuiltinByteArray
  let freeVars = HashSet.fromList [dataT]
  unless
    ((ftype ==% (u <>--> dataT --> byteArray --> byteArray)) freeVars)
    $ builtinsErrorText (getLoc f) "anomaSignDetached must be of type {A : Type} -> A -> ByteArray -> ByteArray"

checkAnomaByteArrayToAnomaContents :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaByteArrayToAnomaContents f = do
  let ftype = f ^. axiomType
      l = getLoc f
  byteArray <- getBuiltinNameScoper l BuiltinByteArray
  nat_ <- getBuiltinNameScoper l BuiltinNat
  unless
    (ftype == (byteArray --> nat_))
    $ builtinsErrorText l "toAnomaContents must be of type ByteArray -> Nat"

checkAnomaByteArrayFromAnomaContents :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaByteArrayFromAnomaContents f = do
  let ftype = f ^. axiomType
      l = getLoc f
  byteArray <- getBuiltinNameScoper l BuiltinByteArray
  nat_ <- getBuiltinNameScoper l BuiltinNat
  unless
    (ftype == (nat_ --> nat_ --> byteArray))
    $ builtinsErrorText l "fromAnomaContents must be of type Nat -> Nat -> ByteArray"

checkAnomaSha256 :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaSha256 f = do
  let ftype = f ^. axiomType
      l = getLoc f
  byteArray <- getBuiltinNameScoper l BuiltinByteArray
  nat_ <- getBuiltinNameScoper l BuiltinNat
  unless
    (ftype == (nat_ --> byteArray))
    $ builtinsErrorText l "anomaSha256 must be of type Nat -> ByteArray"
