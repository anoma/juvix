module Juvix.Compiler.Builtins.Assert where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkAssert :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkAssert f = do
  let ftype = f ^. funDefType
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  bool_ <- getBuiltinNameScoper (getLoc f) BuiltinBool
  valueT <- freshVar l "valueT"
  let freeVars = hashSet [valueT]
  unless ((ftype ==% (u <>--> bool_ --> valueT --> valueT)) freeVars) $
    builtinsErrorText (getLoc f) "assert must be of type {Value : Type} -> Bool -> Value -> Value"
