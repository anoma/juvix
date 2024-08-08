module Juvix.Compiler.Builtins.Nat where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude

checkNatDef :: forall r. (Members '[Builtins, Error BuiltinsError] r) => InductiveDef -> Sem r ()
checkNatDef d = do
  let err :: forall a. Text -> Sem r a
      err = builtinsErrorText (getLoc d)
  unless (null (d ^. inductiveParameters)) (err "Nats should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (err "Nats should be in the small universe")
  case d ^. inductiveConstructors of
    [c1, c2] -> checkZero c1 >> checkSuc c2
    _ -> err "Nat numbers should have exactly two constructors"

checkZero :: (Members '[Builtins, Error BuiltinsError] r) => ConstructorDef -> Sem r ()
checkZero d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  nat <- getBuiltinName (getLoc d) BuiltinNat
  unless (ty === nat) $
    builtinsErrorMsg (getLoc d) $
      "zero has the wrong type " <> ppOutDefault ty <> " | " <> ppOutDefault nat

checkSuc :: (Members '[Builtins, Error BuiltinsError] r) => ConstructorDef -> Sem r ()
checkSuc d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  nat <- getBuiltinName (getLoc d) BuiltinNat
  unless (ty === (nat --> nat)) $
    builtinsErrorText (getLoc d) "suc has the wrong type"

checkNatPrint :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkNatPrint f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  io <- getBuiltinName (getLoc f) BuiltinIO
  unless (f ^. axiomType === (nat --> io)) $
    builtinsErrorText (getLoc f) "Nat print has the wrong type signature"

checkNatPlus :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatPlus f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  let plus = f ^. funDefName
      l = getLoc f
  varn <- freshVar l "n"
  varm <- freshVar l "m"
  let n = toExpression varn
      m = toExpression varm
      (.+.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .+. y = plus @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (zero .+. m, m),
          ((suc @@ n) .+. m, suc @@ (n .+. m))
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatPlus,
        _funInfoSignature = nat --> nat --> nat,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

checkNatMul :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatMul f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  plus <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatPlus
  let mul = f ^. funDefName
      l = getLoc f
  varn <- freshVar l "n"
  varm <- freshVar l "m"
  h <- freshHole l
  let n = toExpression varn
      m = toExpression varm
      (.*.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .*. y = mul @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (zero .*. h, zero),
          ((suc @@ n) .*. m, plus @@ m @@ (n .*. m))
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatMul,
        _funInfoSignature = nat --> nat --> nat,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

checkNatSub :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatSub f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  let sub = f ^. funDefName
      l = getLoc f
  varn <- freshVar l "n"
  varm <- freshVar l "m"
  h <- freshHole l
  let n = toExpression varn
      m = toExpression varm
      (.-.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .-. y = sub @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (zero .-. h, zero),
          (n .-. zero, n),
          ((suc @@ n) .-. (suc @@ m), n .-. m)
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatSub,
        _funInfoSignature = nat --> nat --> nat,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

checkNatUDiv :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatUDiv f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  sub <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSub
  let divop = f ^. funDefName
      l = getLoc l
  varn <- freshVar l "n"
  varm <- freshVar l "m"
  h <- freshHole l
  let n = toExpression varn
      m = toExpression varm
      (./.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x ./. y = divop @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (zero ./. h, zero),
          (n ./. m, suc @@ ((sub @@ n @@ m) ./. m))
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatUDiv,
        _funInfoSignature = nat --> nat --> nat,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

checkNatDiv :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatDiv f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  udiv <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatUDiv
  sub <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSub
  let divop = f ^. funDefName
      l = getLoc f
  varn <- freshVar l "n"
  varm <- freshVar l "m"
  let n = toExpression varn
      m = toExpression varm
      (./.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x ./. y = divop @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (n ./. m, udiv @@ (sub @@ (suc @@ n) @@ m) @@ m)
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatDiv,
        _funInfoSignature = nat --> nat --> nat,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

checkNatMod :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatMod f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  sub <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSub
  divop <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatDiv
  mul <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatMul
  let modop = f ^. funDefName
      l = getLoc f
  varn <- freshVar l "n"
  varm <- freshVar l "m"
  let n = toExpression varn
      m = toExpression varm
      exClauses =
        [ (modop @@ n @@ m, sub @@ n @@ (mul @@ (divop @@ n @@ m) @@ m))
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatMod,
        _funInfoSignature = nat --> nat --> nat,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

checkNatLe :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatLe f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  tybool <- getBuiltinName (getLoc f) BuiltinBool
  true <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolTrue
  false <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolFalse
  let le = f ^. funDefName
      l = getLoc f
  varn <- freshVar l "n"
  varm <- freshVar l "m"
  h <- freshHole l
  let n = toExpression varn
      m = toExpression varm
      (.<=.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .<=. y = le @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (zero .<=. h, true),
          (h .<=. zero, false),
          ((suc @@ n) .<=. (suc @@ m), n .<=. m)
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatLe,
        _funInfoSignature = nat --> nat --> tybool,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

checkNatLt :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatLt f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  le <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatLe
  tybool <- getBuiltinName (getLoc f) BuiltinBool
  let lt = f ^. funDefName
      l = getLoc f
  varn <- freshVar l "n"
  varm <- freshVar l "m"
  let n = toExpression varn
      m = toExpression varm
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (lt @@ n @@ m, le @@ (suc @@ n) @@ m)
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatLt,
        _funInfoSignature = nat --> nat --> tybool,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

checkNatEq :: (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatEq f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  tybool <- getBuiltinName (getLoc f) BuiltinBool
  true <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolTrue
  false <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolFalse
  let eq = f ^. funDefName
      l = getLoc f
  varn <- freshVar l "n"
  varm <- freshVar l "m"
  h <- freshHole l
  let n = toExpression varn
      m = toExpression varm
      (.==.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .==. y = eq @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (zero .==. zero, true),
          (zero .==. h, false),
          (h .==. zero, false),
          ((suc @@ n) .==. (suc @@ m), n .==. m)
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatEq,
        _funInfoSignature = nat --> nat --> tybool,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

checkFromNat :: FunctionDef -> Sem r ()
checkFromNat _ = return ()
