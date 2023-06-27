module Juvix.Compiler.Builtins.Debug where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Builtins.Effect
import Juvix.Prelude

registerTrace :: Members '[Builtins, NameIdGen] r => AxiomDef -> Sem r ()
registerTrace f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  a <- freshVar l "a"
  let freeVars = HashSet.fromList [a]
  unless
    ((ftype ==% (u <>--> a --> a)) freeVars)
    (error "trace must be of type {A : Type} -> A -> A")
  registerBuiltin BuiltinTrace (f ^. axiomName)

registerFail :: Members '[Builtins, NameIdGen] r => AxiomDef -> Sem r ()
registerFail f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  a <- freshVar l "a"
  let freeVars = HashSet.fromList [a]
  string_ <- getBuiltinName (getLoc f) BuiltinString
  unless
    ((ftype ==% (u <>--> string_ --> a)) freeVars)
    (error "fail must be of type {A : Type} -> String -> A")
  registerBuiltin BuiltinFail (f ^. axiomName)
