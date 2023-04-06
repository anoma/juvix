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
  varm <- freshVar "m"
  varn <- freshVar "n"
  h1 <- freshHole
  h2 <- freshHole
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
    builtinName :: (IsBuiltin a) => a -> Sem r Name
    builtinName = getBuiltinName (getLoc f)
