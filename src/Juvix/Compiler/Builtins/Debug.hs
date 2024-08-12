module Juvix.Compiler.Builtins.Debug where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkTrace :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkTrace f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  a <- freshVar l "a"
  let freeVars = HashSet.fromList [a]
  unless ((ftype ==% (u <>--> a --> a)) freeVars) $
    builtinsErrorText (getLoc f) "trace must be of type {A : Type} -> A -> A"

checkFail :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => AxiomDef -> Sem r ()
checkFail f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  a <- freshVar l "a"
  let freeVars = HashSet.fromList [a]
  string_ <- getBuiltinNameScoper (getLoc f) BuiltinString
  unless ((ftype ==% (u <>--> string_ --> a)) freeVars) $
    builtinsErrorText (getLoc f) "fail must be of type {A : Type} -> String -> A"
