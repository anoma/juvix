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
