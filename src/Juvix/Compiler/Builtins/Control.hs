module Juvix.Compiler.Builtins.Control where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

registerSeq :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerSeq f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinSeq,
        _funInfoSignature = u <>--> u <>--> a --> b --> b,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = [a, b]
      }
