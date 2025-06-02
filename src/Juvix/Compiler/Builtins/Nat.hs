module Juvix.Compiler.Builtins.Nat where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude

checkNatDef :: forall r. (Members '[Reader BuiltinsTable, Error ScoperError] r) => InductiveDef -> Sem r ()
checkNatDef d = do
  let err :: forall a. Text -> Sem r a
      err = builtinsErrorText (getLoc d)
  unless (null (d ^. inductiveParameters)) (err "Nats should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (err "Nats should be in the small universe")
  case d ^. inductiveConstructors of
    [c1, c2] -> checkZero c1 >> checkSuc c2
    _ -> err "Nat numbers should have exactly two constructors"

checkZero :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => ConstructorDef -> Sem r ()
checkZero d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  nat <- getBuiltinNameScoper (getLoc d) BuiltinNat
  unless (ty === nat)
    $ builtinsErrorMsg (getLoc d)
    $ "zero has the wrong type "
    <> ppOutDefault ty
    <> " | "
    <> ppOutDefault nat

checkSuc :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => ConstructorDef -> Sem r ()
checkSuc d@ConstructorDef {..} = do
  let ty = _inductiveConstructorType
  nat <- getBuiltinNameScoper (getLoc d) BuiltinNat
  unless (ty === (nat --> nat))
    $ builtinsErrorText (getLoc d) "suc has the wrong type"

checkNatPrint :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkNatPrint f = do
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  io <- getBuiltinNameScoper (getLoc f) BuiltinIO
  unless (f ^. axiomType === (nat --> io))
    $ builtinsErrorText (getLoc f) "Nat print has the wrong type signature"

checkNatPlus :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatPlus f = do
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatSuc
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

checkNatMul :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatMul f = do
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatSuc
  plus <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatPlus
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

checkNatSub :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatSub f = do
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatSuc
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

checkNatUDiv :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatUDiv f = do
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatSuc
  sub <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatSub
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

checkNatDiv :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatDiv f = do
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  suc <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatSuc
  udiv <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatUDiv
  sub <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatSub
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

checkNatMod :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatMod f = do
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  sub <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatSub
  divop <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatDiv
  mul <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatMul
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

checkNatLe :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatLe f = do
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatSuc
  tybool <- getBuiltinNameScoper (getLoc f) BuiltinBool
  true <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinBoolTrue
  false <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinBoolFalse
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

checkNatLt :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatLt f = do
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  suc <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatSuc
  le <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatLe
  tybool <- getBuiltinNameScoper (getLoc f) BuiltinBool
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

checkNatEq :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkNatEq f = do
  nat <- getBuiltinNameScoper (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinNatSuc
  tybool <- getBuiltinNameScoper (getLoc f) BuiltinBool
  true <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinBoolTrue
  false <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinBoolFalse
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
