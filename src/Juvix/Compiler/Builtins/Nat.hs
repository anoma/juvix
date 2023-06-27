module Juvix.Compiler.Builtins.Nat where

import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Abstract.Pretty
import Juvix.Compiler.Builtins.Effect
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude

registerNatDef :: (Member Builtins r) => InductiveDef -> Sem r ()
registerNatDef d = do
  unless (null (d ^. inductiveParameters)) (error "Nats should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (error "Nats should be in the small universe")
  registerBuiltin BuiltinNat (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c1, c2] -> registerZero c1 >> registerSuc c2
    _ -> error "Nat numbers should have exactly two constructors"

registerZero :: (Member Builtins r) => InductiveConstructorDef -> Sem r ()
registerZero d@InductiveConstructorDef {..} = do
  let zero = _constructorName
      ty = _constructorType
  nat <- getBuiltinName (getLoc d) BuiltinNat
  unless (ty === nat) (error $ "zero has the wrong type " <> ppTrace ty <> " | " <> ppTrace nat)
  registerBuiltin BuiltinNatZero zero

registerSuc :: (Member Builtins r) => InductiveConstructorDef -> Sem r ()
registerSuc d@InductiveConstructorDef {..} = do
  let suc = _constructorName
      ty = _constructorType
  nat <- getBuiltinName (getLoc d) BuiltinNat
  unless (ty === (nat --> nat)) (error "suc has the wrong type")
  registerBuiltin BuiltinNatSuc suc

registerNatPrint :: (Members '[Builtins] r) => AxiomDef -> Sem r ()
registerNatPrint f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  io <- getBuiltinName (getLoc f) BuiltinIO
  unless (f ^. axiomType === (nat --> io)) (error "Nat print has the wrong type signature")
  registerBuiltin BuiltinNatPrint (f ^. axiomName)

registerNatPlus :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatPlus f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatPlus,
        _funInfoSignature = nat --> nat --> nat,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

registerNatMul :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatMul f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatMul,
        _funInfoSignature = nat --> nat --> nat,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

registerNatSub :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatSub f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatSub,
        _funInfoSignature = nat --> nat --> nat,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

registerNatUDiv :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatUDiv f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatUDiv,
        _funInfoSignature = nat --> nat --> nat,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

registerNatDiv :: Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerNatDiv f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatDiv,
        _funInfoSignature = nat --> nat --> nat,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

registerNatMod :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatMod f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatMod,
        _funInfoSignature = nat --> nat --> nat,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

registerNatLe :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatLe f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatLe,
        _funInfoSignature = nat --> nat --> tybool,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

registerNatLt :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatLt f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatLt,
        _funInfoSignature = nat --> nat --> tybool,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }

registerNatEq :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatEq f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinNatEq,
        _funInfoSignature = nat --> nat --> tybool,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }
