module Juvix.Compiler.Builtins.Control where

import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Builtins.Effect
import Juvix.Prelude

registerSeq :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerSeq f = do
  let u = ExpressionUniverse (Universe {_universeLevel = Nothing, _universeLoc = error "Universe with no location"})
  a <- freshVar "a"
  b <- freshVar "b"
  let seq = f ^. funDefName
  varn <- freshVar "n"
  varm <- freshVar "m"
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
