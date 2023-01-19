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

registerNatFun ::
  (Members '[Builtins, NameIdGen] r) =>
  FunctionDef ->
  BuiltinFunction ->
  Expression ->
  [(Expression, Expression)] ->
  [VarName] ->
  Sem r ()
registerNatFun f blt sig exClauses fvs = do
  let op = f ^. funDefName
      ty = f ^. funDefTypeSig
  unless (ty === sig) (error "builtin has the wrong type signature")
  registerBuiltin blt op
  let freeVars = HashSet.fromList fvs
      a =% b = (a ==% b) freeVars
      clauses :: [(Expression, Expression)]
      clauses =
        [ (clauseLhsAsExpression c, c ^. clauseBody)
          | c <- toList (f ^. funDefClauses)
        ]
  case zipExactMay exClauses clauses of
    Nothing -> error "builtin has the wrong number of clauses"
    Just z -> forM_ z $ \((exLhs, exBody), (lhs, body)) -> do
      unless (exLhs =% lhs) (error "clause lhs does not match")
      unless (exBody =% body) (error $ "clause body does not match " <> ppTrace exBody <> " | " <> ppTrace body)

registerNatPlus :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatPlus f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  let plus = f ^. funDefName
  varn <- freshVar "n"
  varm <- freshVar "m"
  let n = toExpression varn
      m = toExpression varm
      (.+.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .+. y = plus @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (zero .+. m, m),
          ((suc @@ n) .+. m, suc @@ (n .+. m))
        ]
  registerFun f BuiltinNatPlus (nat --> nat --> nat) exClauses [varn, varm] []

registerNatMul :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatMul f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  plus <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatPlus
  let mul = f ^. funDefName
  varn <- freshVar "n"
  varm <- freshVar "m"
  h <- freshHole
  let n = toExpression varn
      m = toExpression varm
      (.*.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .*. y = mul @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (zero .*. h, zero),
          ((suc @@ n) .*. m, plus @@ m @@ (n .*. m))
        ]
  registerFun f BuiltinNatMul (nat --> nat --> nat) exClauses [varn, varm] []

registerNatSub :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatSub f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  let sub = f ^. funDefName
  varn <- freshVar "n"
  varm <- freshVar "m"
  h <- freshHole
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
  registerFun f BuiltinNatSub (nat --> nat --> nat) exClauses [varn, varm] []

registerNatUDiv :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatUDiv f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  sub <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSub
  let divop = f ^. funDefName
  varn <- freshVar "n"
  varm <- freshVar "m"
  h <- freshHole
  let n = toExpression varn
      m = toExpression varm
      (./.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x ./. y = divop @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (zero ./. h, zero),
          (n ./. m, suc @@ ((sub @@ n @@ m) ./. m))
        ]
  registerFun f BuiltinNatUDiv (nat --> nat --> nat) exClauses [varn, varm] []

registerNatDiv :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatDiv f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  udiv <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatUDiv
  sub <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSub
  let divop = f ^. funDefName
  varn <- freshVar "n"
  varm <- freshVar "m"
  let n = toExpression varn
      m = toExpression varm
      (./.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x ./. y = divop @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (n ./. m, udiv @@ (sub @@ (suc @@ n) @@ m) @@ m)
        ]
  registerFun f BuiltinNatDiv (nat --> nat --> nat) exClauses [varn, varm] []

registerNatMod :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatMod f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  sub <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSub
  divop <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatDiv
  mul <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatMul
  let modop = f ^. funDefName
  varn <- freshVar "n"
  varm <- freshVar "m"
  let n = toExpression varn
      m = toExpression varm
      exClauses =
        [ (modop @@ n @@ m, sub @@ n @@ (mul @@ (divop @@ n @@ m) @@ m))
        ]
  registerFun f BuiltinNatMod (nat --> nat --> nat) exClauses [varn, varm] []

registerNatLe :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatLe f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  tybool <- getBuiltinName (getLoc f) BuiltinBool
  true <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolTrue
  false <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolFalse
  let le = f ^. funDefName
  varn <- freshVar "n"
  varm <- freshVar "m"
  h <- freshHole
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
  registerFun f BuiltinNatLe (nat --> nat --> tybool) exClauses [varn, varm] []

registerNatLt :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatLt f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  le <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatLe
  tybool <- getBuiltinName (getLoc f) BuiltinBool
  let lt = f ^. funDefName
  varn <- freshVar "n"
  varm <- freshVar "m"
  let n = toExpression varn
      m = toExpression varm
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (lt @@ n @@ m, le @@ (suc @@ n) @@ m)
        ]
  registerFun f BuiltinNatLt (nat --> nat --> tybool) exClauses [varn, varm] []

registerNatEq :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerNatEq f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  tybool <- getBuiltinName (getLoc f) BuiltinBool
  true <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolTrue
  false <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolFalse
  let eq = f ^. funDefName
  varn <- freshVar "n"
  varm <- freshVar "m"
  h <- freshHole
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
  registerFun f BuiltinNatEq (nat --> nat --> tybool) exClauses [varn, varm] []
