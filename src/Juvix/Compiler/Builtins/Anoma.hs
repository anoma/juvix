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
