module Juvix.Compiler.Builtins.Debug where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Builtins.Effect
import Juvix.Prelude

registerTrace :: (Members '[Builtins, NameIdGen] r) => AxiomDef -> Sem r ()
registerTrace f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse (Universe {_universeLevel = Nothing, _universeLoc = error "Universe with no location"})
  a <- freshVar "a"
  b <- freshVar "b"
  let freeVars = HashSet.fromList [a, b]
  unless
    (((u <>--> u <>--> a --> b --> b) ==% ftype) freeVars)
    (error "trace must be of type {A : Type} -> {B : Type} -> A -> B -> B")
  registerBuiltin BuiltinTrace (f ^. axiomName)

registerFail :: (Members '[Builtins, NameIdGen] r) => AxiomDef -> Sem r ()
registerFail f = do
  let ftype = f ^. axiomType
      u = ExpressionUniverse (Universe {_universeLevel = Nothing, _universeLoc = error "Universe with no location"})
  a <- freshVar "a"
  let freeVars = HashSet.fromList [a]
  string_ <- getBuiltinName (getLoc f) BuiltinString
  unless
    (((u <>--> string_ --> a) ==% ftype) freeVars)
    (error "fail must be of type {A : Type} -> String -> A")
  registerBuiltin BuiltinFail (f ^. axiomName)
