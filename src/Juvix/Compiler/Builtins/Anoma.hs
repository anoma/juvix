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

checkResource :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => InductiveDef -> Sem r ()
checkResource d = do
  let err = builtinsErrorText (getLoc d)
  unless (null (d ^. inductiveParameters)) (err "AnomaResource should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (err "AnomaResource should be in the small universe")
  unless (length (d ^. inductiveConstructors) == 1) (err "AnomaResource should have exactly one constructor")

checkAction :: (Members '[Error ScoperError] r) => InductiveDef -> Sem r ()
checkAction d = do
  let err = builtinsErrorText (getLoc d)
  unless (null (d ^. inductiveParameters)) (err "AnomaAction should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (err "AnomaAction should be in the small universe")
  unless (length (d ^. inductiveConstructors) == 1) (err "AnomaAction should have exactly one constructor")

checkDelta :: (Members '[Error ScoperError] r) => AxiomDef -> Sem r ()
checkDelta d =
  unless (isSmallUniverse' (d ^. axiomType)) $
    builtinsErrorText (getLoc d) "AnomaDelta should be in the small universe"

checkKind :: (Members '[Error ScoperError] r) => AxiomDef -> Sem r ()
checkKind d =
  unless (isSmallUniverse' (d ^. axiomType)) $
    builtinsErrorText (getLoc d) "AnomaDelta should be in the small universe"

checkResourceCommitment :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkResourceCommitment f = do
  let l = getLoc f
  resource <- getBuiltinNameScoper (getLoc f) BuiltinAnomaResource
  nat_ <- getBuiltinNameScoper l BuiltinNat
  unless (f ^. axiomType === (resource --> nat_)) $
    builtinsErrorText (getLoc f) "resourceCommitment must be of type AnomaResource -> Nat"

checkResourceNullifier :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkResourceNullifier f = do
  let l = getLoc f
  resource <- getBuiltinNameScoper l BuiltinAnomaResource
  nat_ <- getBuiltinNameScoper l BuiltinNat
  unless (f ^. axiomType === (resource --> nat_)) $
    builtinsErrorText l "resourceNullifier must be of type AnomaResource -> Nat"

checkResourceKind :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkResourceKind f = do
  let l = getLoc f
  resource <- getBuiltinNameScoper l BuiltinAnomaResource
  kind <- getBuiltinNameScoper l BuiltinAnomaKind
  unless (f ^. axiomType === (resource --> kind)) $
    builtinsErrorText l "resourceNullifier must be of type AnomaResource -> Nat"

checkResourceDelta :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkResourceDelta f = do
  let l = getLoc f
  resource <- getBuiltinNameScoper l BuiltinAnomaResource
  delta <- getBuiltinNameScoper l BuiltinAnomaDelta
  unless (f ^. axiomType === (resource --> delta)) $
    builtinsErrorText l "resourceDelta must be of type AnomaResource -> AnomaDelta"

checkProveAction :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkProveAction f = do
  let l = getLoc f
  action <- getBuiltinNameScoper l BuiltinAnomaAction
  nat_ <- getBuiltinNameScoper l BuiltinNat
  unless (f ^. axiomType === (action --> nat_)) $
    builtinsErrorText l "proveAction must be of type AnomaAction -> Nat"

checkProveDelta :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkProveDelta f = do
  let l = getLoc f
  delta <- getBuiltinNameScoper l BuiltinAnomaDelta
  nat_ <- getBuiltinNameScoper l BuiltinNat
  unless (f ^. axiomType === (delta --> nat_)) $
    builtinsErrorText l "proveDelta must be of type AnomaDelta -> Nat"

checkActionDelta :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkActionDelta f = do
  let l = getLoc f
  action <- getBuiltinNameScoper l BuiltinAnomaAction
  delta <- getBuiltinNameScoper l BuiltinAnomaDelta
  unless (f ^. axiomType === (action --> delta)) $
    builtinsErrorText l "actionDelta must be of type AnomaAction -> AnomaDelta"

checkActionsDelta :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkActionsDelta f = do
  let l = getLoc f
  action <- getBuiltinNameScoper l BuiltinAnomaAction
  delta <- getBuiltinNameScoper l BuiltinAnomaDelta
  list_ <- getBuiltinNameScoper l BuiltinList
  unless (f ^. axiomType === (list_ @@ action --> delta)) $
    builtinsErrorText l "actionsDelta must be of type List AnomaAction -> AnomaDelta"

checkDeltaBinaryOp :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkDeltaBinaryOp f = do
  let l = getLoc f
  delta <- getBuiltinNameScoper l BuiltinAnomaDelta
  unless (f ^. axiomType === (delta --> delta --> delta)) $
    builtinsErrorText l "deltaAdd must be of type AnomaDelta -> AnomaDelta -> AnomaDelta"

checkZeroDelta :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkZeroDelta f = do
  let l = getLoc f
  delta <- getBuiltinNameScoper l BuiltinAnomaDelta
  unless (f ^. axiomType === delta) $
    builtinsErrorText (getLoc f) "zeroDelta must be of Delta"

checkAnomaRandomGenerator :: (Members '[Error ScoperError] r) => AxiomDef -> Sem r ()
checkAnomaRandomGenerator d =
  unless (isSmallUniverse' (d ^. axiomType)) $
    builtinsErrorText (getLoc d) "AnomaRandomGenerator should be in the small universe"

checkAnomaRandomGeneratorInit :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkAnomaRandomGeneratorInit f = do
  let l = getLoc f
  gen <- getBuiltinNameScoper l BuiltinAnomaRandomGenerator
  nat_ <- getBuiltinNameScoper l BuiltinNat
  unless (f ^. axiomType === (nat_ --> gen)) $
    builtinsErrorText l "initRandomGenerator must be of type Nat -> AnomaRandomGenerator"

checkAnomaRandomNextBytes :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkAnomaRandomNextBytes f = do
  let l = getLoc f
  gen <- getBuiltinNameScoper l BuiltinAnomaRandomGenerator
  nat_ <- getBuiltinNameScoper l BuiltinNat
  bytearray <- getBuiltinNameScoper l BuiltinByteArray
  pair_ <- getBuiltinNameScoper l BuiltinPair
  unless (f ^. axiomType === (nat_ --> gen --> (pair_ @@ bytearray @@ gen))) $
    builtinsErrorText l "nextBytes must be of type Nat -> AnomaRandomGenerator -> Pair ByteArray AnomaRandomGenerator"

checkAnomaRandomSplit :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkAnomaRandomSplit f = do
  let l = getLoc f
  gen <- getBuiltinNameScoper l BuiltinAnomaRandomGenerator
  pair_ <- getBuiltinNameScoper l BuiltinPair
  unless (f ^. axiomType === (gen --> pair_ @@ gen @@ gen)) $
    builtinsErrorText l "randomSplit must be of type AnomaRandomGenerator -> Pair AnomaRandomGenerator AnomaRandomGenerator"

checkAnomaIsCommitment :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkAnomaIsCommitment f = do
  let l = getLoc f
  nat_ <- getBuiltinNameScoper l BuiltinNat
  bool_ <- getBuiltinNameScoper l BuiltinBool
  unless (f ^. axiomType == (nat_ --> bool_)) $
    builtinsErrorText l "isCommitment must be of type Nat -> Bool"

checkAnomaIsNullifier :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkAnomaIsNullifier f = do
  let l = getLoc f
  nat_ <- getBuiltinNameScoper l BuiltinNat
  bool_ <- getBuiltinNameScoper l BuiltinBool
  unless (f ^. axiomType == (nat_ --> bool_)) $
    builtinsErrorText l "isNullifier must be of type Nat -> Bool"

checkAnomaSet :: (Members '[Error ScoperError] r) => AxiomDef -> Sem r ()
checkAnomaSet t = do
  let ty = t ^. axiomType
      l = getLoc t
      u = ExpressionUniverse smallUniverseNoLoc
  unless (ty == (u --> u)) $
    builtinsErrorText l "AnomaSet should have type: Type -> Type"

checkAnomaSetToList :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaSetToList f = do
  let ty = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  elemT <- freshVar l "elemT"
  list_ <- getBuiltinNameScoper l BuiltinList
  anomaSet <- getBuiltinNameScoper l BuiltinAnomaSet
  let freeVars = HashSet.fromList [elemT]
  unless ((ty ==% (u <>--> anomaSet @@ elemT --> list_ @@ elemT)) freeVars) $
    builtinsErrorText l "anomaSetToList should have type: {A : Type} -> AnomaSet A -> List A"

checkAnomaSetFromList :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkAnomaSetFromList f = do
  let ty = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  elemT <- freshVar l "elemT"
  list_ <- getBuiltinNameScoper l BuiltinList
  anomaSet <- getBuiltinNameScoper l BuiltinAnomaSet
  let freeVars = HashSet.fromList [elemT]
  unless ((ty ==% (u <>--> list_ @@ elemT --> anomaSet @@ elemT)) freeVars) $
    builtinsErrorText l "anomaSetFromList should have type: {A : Type} -> List A -> AnomaSet A"
