module Juvix.Compiler.Builtins.Control where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkSeq :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkSeq f = do
  let u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  a <- freshVar l "a"
  b <- freshVar l "b"
  let seq = f ^. funDefName
  varn <- freshVar l "n"
  varm <- freshVar l "m"
  let n = toExpression varn
      m = toExpression varm
      exClauses :: [(Expression, Expression)]
      exClauses = [(seq @@ n @@ m, m)]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinSeq,
        _funInfoSignature = u <>--> u <>--> a --> b --> b,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = [a, b]
      }
