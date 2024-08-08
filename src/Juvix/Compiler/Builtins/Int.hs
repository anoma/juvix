module Juvix.Compiler.Builtins.Int where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

checkIntDef :: (Members '[Builtins, Error BuiltinsError] r) => InductiveDef -> Sem r ()
checkIntDef d = do
  let err msg = builtinsErrorText (getLoc d) msg
  unless (null (d ^. inductiveParameters)) (err "Int should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (err "Int should be in the small universe")
  case d ^. inductiveConstructors of
    [c1, c2] -> checkIntCtor BuiltinIntOfNat c1 >> checkIntCtor BuiltinIntNegSuc c2
    _ -> err "Int should have exactly two constructors"

checkIntCtor :: (Members '[Builtins, Error BuiltinsError] r) => BuiltinConstructor -> ConstructorDef -> Sem r ()
checkIntCtor _ctor d@ConstructorDef {..} = do
  let ctorName = _inductiveConstructorName
      ty = _inductiveConstructorType
      loc = getLoc d
  int <- getBuiltinName loc BuiltinInt
  nat <- getBuiltinName loc BuiltinNat
  unless (ty === (nat --> int)) $
    builtinsErrorText (getLoc d) (ctorName ^. nameText <> " has the wrong type")

checkIntToString :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkIntToString f = do
  string_ <- getBuiltinName (getLoc f) BuiltinString
  int <- getBuiltinName (getLoc f) BuiltinInt
  unless (f ^. axiomType === (int --> string_)) $
    builtinsErrorText (getLoc f) "intToString has the wrong type signature"

checkIntEq :: forall r. (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkIntEq f = do
  int <- builtinName BuiltinInt
  ofNat <- toExpression <$> builtinName BuiltinIntOfNat
  negSuc <- toExpression <$> builtinName BuiltinIntNegSuc
  tybool <- builtinName BuiltinBool
  false <- toExpression <$> builtinName BuiltinBoolFalse
  natEq <- toExpression <$> builtinName BuiltinNatEq
  let l = getLoc f
  varm <- freshVar l "m"
  varn <- freshVar l "n"
  h1 <- freshHole l
  h2 <- freshHole l
  let eq = f ^. funDefName
      m = toExpression varm
      n = toExpression varn
      (.==.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .==. y = eq @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ ((ofNat @@ m) .==. (ofNat @@ n), (natEq @@ m @@ n)),
          ((negSuc @@ m) .==. (negSuc @@ n), (natEq @@ m @@ n)),
          (h1 .==. h2, false)
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntEq,
        _funInfoSignature = int --> int --> tybool,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: (IsBuiltin a) => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

checkIntSubNat :: forall r. (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkIntSubNat f = do
  let loc = getLoc f
  int <- getBuiltinName loc BuiltinInt
  nat <- getBuiltinName loc BuiltinNat
  unless (f ^. funDefType === (nat --> nat --> int)) $
    builtinsErrorText (getLoc f) "int-sub-nat has the wrong type signature"

checkIntPlus :: forall r. (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkIntPlus f = do
  let loc = getLoc f
  int <- getBuiltinName loc BuiltinInt
  ofNat <- toExpression <$> getBuiltinName loc BuiltinIntOfNat
  negSuc <- toExpression <$> getBuiltinName loc BuiltinIntNegSuc
  suc <- toExpression <$> getBuiltinName loc BuiltinNatSuc
  natPlus <- toExpression <$> getBuiltinName loc BuiltinNatPlus
  intSubNat <- toExpression <$> getBuiltinName loc BuiltinIntSubNat
  let plus = f ^. funDefName
      l = getLoc f
  varn <- freshVar l "m"
  varm <- freshVar l "n"
  let m = toExpression varm
      n = toExpression varn
      (.+.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .+. y = plus @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ ((ofNat @@ m) .+. (ofNat @@ n), ofNat @@ (natPlus @@ m @@ n)),
          ((ofNat @@ m) .+. (negSuc @@ n), intSubNat @@ m @@ (suc @@ n)),
          ((negSuc @@ m) .+. (ofNat @@ n), intSubNat @@ n @@ (suc @@ m)),
          ((negSuc @@ m) .+. (negSuc @@ n), negSuc @@ (suc @@ (natPlus @@ m @@ n)))
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntPlus,
        _funInfoSignature = int --> int --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }

checkIntNegNat :: forall r. (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkIntNegNat f = do
  int <- builtinName BuiltinInt
  nat <- builtinName BuiltinNat
  ofNat <- toExpression <$> builtinName BuiltinIntOfNat
  negSuc <- toExpression <$> builtinName BuiltinIntNegSuc
  zero <- toExpression <$> builtinName BuiltinNatZero
  suc <- toExpression <$> builtinName BuiltinNatSuc
  let l = getLoc f
  varn <- freshVar l "n"
  let negNat = f ^. funDefName
      n = toExpression varn
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (negNat @@ zero, ofNat @@ zero),
          (negNat @@ (suc @@ n), negSuc @@ n)
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntNegNat,
        _funInfoSignature = nat --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: (IsBuiltin a) => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

checkIntNeg :: forall r. (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkIntNeg f = do
  int <- builtinName BuiltinInt
  ofNat <- toExpression <$> builtinName BuiltinIntOfNat
  negSuc <- toExpression <$> builtinName BuiltinIntNegSuc
  negNat <- toExpression <$> builtinName BuiltinIntNegNat
  suc <- toExpression <$> builtinName BuiltinNatSuc
  let l = getLoc f
  varn <- freshVar l "n"
  let neg = f ^. funDefName
      n = toExpression varn
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (neg @@ (ofNat @@ n), negNat @@ n),
          (neg @@ (negSuc @@ n), ofNat @@ (suc @@ n))
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntNeg,
        _funInfoSignature = int --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: (IsBuiltin a) => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

checkIntMul :: forall r. (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkIntMul f = do
  int <- builtinName BuiltinInt
  ofNat <- toExpression <$> builtinName BuiltinIntOfNat
  negSuc <- toExpression <$> builtinName BuiltinIntNegSuc
  negNat <- toExpression <$> builtinName BuiltinIntNegNat
  natMul <- toExpression <$> builtinName BuiltinNatMul
  natSuc <- toExpression <$> builtinName BuiltinNatSuc
  let l = getLoc f
  varm <- freshVar l "m"
  varn <- freshVar l "n"
  let mul = f ^. funDefName
      m = toExpression varm
      n = toExpression varn
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (mul @@ (ofNat @@ m) @@ (ofNat @@ n), ofNat @@ (natMul @@ m @@ n)),
          (mul @@ (ofNat @@ m) @@ (negSuc @@ n), negNat @@ (natMul @@ m @@ (natSuc @@ n))),
          (mul @@ (negSuc @@ m) @@ (ofNat @@ n), negNat @@ (natMul @@ (natSuc @@ m) @@ n)),
          (mul @@ (negSuc @@ m) @@ (negSuc @@ n), ofNat @@ (natMul @@ (natSuc @@ m) @@ (natSuc @@ n)))
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntMul,
        _funInfoSignature = int --> int --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: (IsBuiltin a) => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

checkIntDiv :: forall r. (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkIntDiv f = do
  int <- builtinName BuiltinInt
  ofNat <- toExpression <$> builtinName BuiltinIntOfNat
  negSuc <- toExpression <$> builtinName BuiltinIntNegSuc
  negNat <- toExpression <$> builtinName BuiltinIntNegNat
  natDiv <- toExpression <$> builtinName BuiltinNatDiv
  natSuc <- toExpression <$> builtinName BuiltinNatSuc
  let l = getLoc f
  varm <- freshVar l "m"
  varn <- freshVar l "n"
  let intDiv = f ^. funDefName
      m = toExpression varm
      n = toExpression varn
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (intDiv @@ (ofNat @@ m) @@ (ofNat @@ n), ofNat @@ (natDiv @@ m @@ n)),
          (intDiv @@ (ofNat @@ m) @@ (negSuc @@ n), negNat @@ (natDiv @@ m @@ (natSuc @@ n))),
          (intDiv @@ (negSuc @@ m) @@ (ofNat @@ n), negNat @@ (natDiv @@ (natSuc @@ m) @@ n)),
          (intDiv @@ (negSuc @@ m) @@ (negSuc @@ n), ofNat @@ (natDiv @@ (natSuc @@ m) @@ (natSuc @@ n)))
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntDiv,
        _funInfoSignature = int --> int --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: (IsBuiltin a) => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

checkIntMod :: forall r. (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkIntMod f = do
  int <- builtinName BuiltinInt
  ofNat <- toExpression <$> builtinName BuiltinIntOfNat
  negSuc <- toExpression <$> builtinName BuiltinIntNegSuc
  negNat <- toExpression <$> builtinName BuiltinIntNegNat
  natMod <- toExpression <$> builtinName BuiltinNatMod
  natSuc <- toExpression <$> builtinName BuiltinNatSuc
  let l = getLoc f
  varm <- freshVar l "m"
  varn <- freshVar l "n"
  let intMod = f ^. funDefName
      m = toExpression varm
      n = toExpression varn
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (intMod @@ (ofNat @@ m) @@ (ofNat @@ n), ofNat @@ (natMod @@ m @@ n)),
          (intMod @@ (ofNat @@ m) @@ (negSuc @@ n), ofNat @@ (natMod @@ m @@ (natSuc @@ n))),
          (intMod @@ (negSuc @@ m) @@ (ofNat @@ n), negNat @@ (natMod @@ (natSuc @@ m) @@ n)),
          (intMod @@ (negSuc @@ m) @@ (negSuc @@ n), negNat @@ (natMod @@ (natSuc @@ m) @@ (natSuc @@ n)))
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntMod,
        _funInfoSignature = int --> int --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: (IsBuiltin a) => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

checkIntSub :: forall r. (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkIntSub f = do
  int <- builtinName BuiltinInt
  neg <- toExpression <$> builtinName BuiltinIntNeg
  intPlus <- toExpression <$> builtinName BuiltinIntPlus
  let l = getLoc f
  varm <- freshVar l "m"
  varn <- freshVar l "n"
  let intSub = f ^. funDefName
      m = toExpression varm
      n = toExpression varn
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (intSub @@ m @@ n, intPlus @@ m @@ (neg @@ n))
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntSub,
        _funInfoSignature = int --> int --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: (IsBuiltin a) => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

checkIntNonNeg :: forall r. (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkIntNonNeg f = do
  int <- builtinName BuiltinInt
  bool_ <- builtinName BuiltinBool
  ofNat <- toExpression <$> builtinName BuiltinIntOfNat
  negSuc <- toExpression <$> builtinName BuiltinIntNegSuc
  true <- toExpression <$> builtinName BuiltinBoolTrue
  false <- toExpression <$> builtinName BuiltinBoolFalse
  let l = getLoc l
  varn <- freshVar l "n"
  h <- freshHole l
  let intNonNeg = f ^. funDefName
      n = toExpression varn

      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (intNonNeg @@ (ofNat @@ n), true),
          (intNonNeg @@ (negSuc @@ h), false)
        ]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntNonNeg,
        _funInfoSignature = int --> bool_,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: (IsBuiltin a) => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

checkIntPrint :: (Members '[Builtins, Error BuiltinsError] r) => AxiomDef -> Sem r ()
checkIntPrint f = do
  int <- getBuiltinName (getLoc f) BuiltinInt
  io <- getBuiltinName (getLoc f) BuiltinIO
  unless (f ^. axiomType === (int --> io)) $
    builtinsErrorText (getLoc f) "Int print has the wrong type signature"

checkIntLe :: forall r. (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkIntLe f = do
  int <- builtinName BuiltinInt
  bool_ <- builtinName BuiltinBool
  nonNeg <- toExpression <$> builtinName BuiltinIntNonNeg
  intSub <- toExpression <$> builtinName BuiltinIntSub
  let l = getLoc f
  varm <- freshVar l "m"
  varn <- freshVar l "n"
  let intLe = f ^. funDefName
      (.-.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .-. y = intSub @@ x @@ y
      m = toExpression varm
      n = toExpression varn
      exClauses :: [(Expression, Expression)]
      exClauses =
        [(intLe @@ m @@ n, nonNeg @@ (n .-. m))]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntLe,
        _funInfoSignature = int --> int --> bool_,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: (IsBuiltin a) => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

checkIntLt :: forall r. (Members '[Builtins, Error BuiltinsError, NameIdGen] r) => FunctionDef -> Sem r ()
checkIntLt f = do
  int <- builtinName BuiltinInt
  bool_ <- builtinName BuiltinBool
  ofNat <- toExpression <$> builtinName BuiltinIntOfNat
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  intLe <- toExpression <$> builtinName BuiltinIntLe
  intPlus <- toExpression <$> builtinName BuiltinIntPlus
  let l = getLoc f
  varm <- freshVar l "m"
  varn <- freshVar l "n"
  let intLt = f ^. funDefName
      (.+.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .+. y = intPlus @@ x @@ y
      (.<=.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      lit1 = ofNat @@ (suc @@ zero)
      x .<=. y = intLe @@ x @@ y
      m = toExpression varm
      n = toExpression varn
      exClauses :: [(Expression, Expression)]
      exClauses =
        [(intLt @@ m @@ n, (m .+. lit1) .<=. n)]
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntLt,
        _funInfoSignature = int --> int --> bool_,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: (IsBuiltin a) => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

checkFromInt :: FunctionDef -> Sem r ()
checkFromInt = const (return ())
