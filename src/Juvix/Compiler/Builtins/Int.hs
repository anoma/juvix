module Juvix.Compiler.Builtins.Int where

import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Builtins.Effect
import Juvix.Prelude

registerIntDef :: Member Builtins r => InductiveDef -> Sem r ()
registerIntDef d = do
  unless (null (d ^. inductiveParameters)) (error "Int should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (error "Int should be in the small universe")
  registerBuiltin BuiltinInt (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c1, c2] -> registerIntCtor BuiltinIntOfNat c1 >> registerIntCtor BuiltinIntNegSuc c2
    _ -> error "Int should have exactly two constructors"

registerIntCtor :: (Member Builtins r) => BuiltinConstructor -> InductiveConstructorDef -> Sem r ()
registerIntCtor ctor d@InductiveConstructorDef {..} = do
  let ctorName = _constructorName
      ty = _constructorType
      loc = getLoc d
  int <- getBuiltinName loc BuiltinInt
  nat <- getBuiltinName loc BuiltinNat
  unless (ty === (nat --> int)) (error (ctorName ^. nameText <> " has the wrong type"))
  registerBuiltin ctor ctorName

registerIntToString :: Member Builtins r => AxiomDef -> Sem r ()
registerIntToString f = do
  string_ <- getBuiltinName (getLoc f) BuiltinString
  int <- getBuiltinName (getLoc f) BuiltinInt
  unless (f ^. axiomType === (int --> string_)) (error "intToString has the wrong type signature")
  registerBuiltin BuiltinIntToString (f ^. axiomName)

registerIntEq :: forall r. Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerIntEq f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntEq,
        _funInfoSignature = int --> int --> tybool,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn, varm],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: IsBuiltin a => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

registerIntSubNat :: forall r. Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerIntSubNat f = do
  let loc = getLoc f
  int <- getBuiltinName loc BuiltinInt
  nat <- getBuiltinName loc BuiltinNat
  unless (f ^. funDefTypeSig === (nat --> nat --> int)) (error "int-sub-nat has the wrong type signature")
  registerBuiltin BuiltinIntSubNat (f ^. funDefName)

registerIntPlus :: forall r. Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerIntPlus f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntPlus,
        _funInfoSignature = int --> int --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }

registerIntNegNat :: forall r. Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerIntNegNat f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntNegNat,
        _funInfoSignature = nat --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: IsBuiltin a => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

registerIntNeg :: forall r. Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerIntNeg f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntNeg,
        _funInfoSignature = int --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: IsBuiltin a => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

registerIntMul :: forall r. Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerIntMul f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntMul,
        _funInfoSignature = int --> int --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: IsBuiltin a => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

registerIntDiv :: forall r. Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerIntDiv f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntDiv,
        _funInfoSignature = int --> int --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: IsBuiltin a => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

registerIntMod :: forall r. Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerIntMod f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntMod,
        _funInfoSignature = int --> int --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: IsBuiltin a => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

registerIntSub :: forall r. Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerIntSub f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntSub,
        _funInfoSignature = int --> int --> int,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: IsBuiltin a => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

registerIntNonNeg :: forall r. Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerIntNonNeg f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntNonNeg,
        _funInfoSignature = int --> bool_,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: IsBuiltin a => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

registerIntPrint :: Members '[Builtins] r => AxiomDef -> Sem r ()
registerIntPrint f = do
  int <- getBuiltinName (getLoc f) BuiltinInt
  io <- getBuiltinName (getLoc f) BuiltinIO
  unless (f ^. axiomType === (int --> io)) (error "Int print has the wrong type signature")
  registerBuiltin BuiltinIntPrint (f ^. axiomName)

registerIntLe :: forall r. Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerIntLe f = do
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
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntLe,
        _funInfoSignature = int --> int --> bool_,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: IsBuiltin a => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)

registerIntLt :: forall r. Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerIntLt f = do
  int <- builtinName BuiltinInt
  bool_ <- builtinName BuiltinBool
  intLe <- toExpression <$> builtinName BuiltinIntLe
  intPlus <- toExpression <$> builtinName BuiltinIntPlus
  let l = getLoc f
  varm <- freshVar l "m"
  varn <- freshVar l "n"
  let intLt = f ^. funDefName
      (.+.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .+. y = intPlus @@ x @@ y
      (.<=.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      lit1 = ExpressionLiteral (WithLoc (getLoc f) (LitInteger 1))
      x .<=. y = intLe @@ x @@ y
      m = toExpression varm
      n = toExpression varn
      exClauses :: [(Expression, Expression)]
      exClauses =
        [(intLt @@ m @@ n, (m .+. lit1) .<=. n)]
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinIntLt,
        _funInfoSignature = int --> int --> bool_,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [varm, varn],
        _funInfoFreeTypeVars = []
      }
  where
    builtinName :: IsBuiltin a => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)
