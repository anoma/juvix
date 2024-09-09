module Juvix.Compiler.Builtins.Assert where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkAssert :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkAssert f = do
  bool_ <- getBuiltinNameScoper (getLoc f) BuiltinBool
  let assert_ = f ^. funDefName
      l = getLoc f
  varx <- freshVar l "x"
  let x = toExpression varx
      assertClauses :: [(Expression, Expression)]
      assertClauses = [(assert_ @@ x, x)]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinAssert,
        _funInfoSignature = bool_ --> bool_,
        _funInfoClauses = assertClauses,
        _funInfoFreeVars = [varx],
        _funInfoFreeTypeVars = []
      }
