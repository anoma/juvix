module Juvix.Compiler.Builtins.Nat where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Abstract.Pretty
import Juvix.Compiler.Builtins.Effect
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude

registerNatDef :: Member Builtins r => InductiveDef -> Sem r ()
registerNatDef d = do
  unless (null (d ^. inductiveParameters)) (error "Nats should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (error "Nats should be in the small universe")
  registerBuiltin BuiltinNat (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c1, c2] -> registerZero c1 >> registerSuc c2
    _ -> error "Nat numbers should have exactly two constructors"

registerZero :: Member Builtins r => InductiveConstructorDef -> Sem r ()
registerZero d@InductiveConstructorDef {..} = do
  let zero = _constructorName
      ty = _constructorType
  nat <- getBuiltinName (getLoc d) BuiltinNat
  unless (ty === nat) (error $ "zero has the wrong type " <> ppTrace ty <> " | " <> ppTrace nat)
  registerBuiltin BuiltinNatZero zero

registerSuc :: Member Builtins r => InductiveConstructorDef -> Sem r ()
registerSuc d@InductiveConstructorDef {..} = do
  let suc = _constructorName
      ty = _constructorType
  nat <- getBuiltinName (getLoc d) BuiltinNat
  unless (ty === (nat --> nat)) (error "suc has the wrong type")
  registerBuiltin BuiltinNatSuc suc

registerNatPrint :: Members '[Builtins] r => AxiomDef -> Sem r ()
registerNatPrint f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  io <- getBuiltinName (getLoc f) BuiltinIO
  unless (f ^. axiomType === (nat --> io)) (error "Nat print has the wrong type signature")
  registerBuiltin BuiltinNatPrint (f ^. axiomName)

registerNatPlus :: Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerNatPlus f = do
  nat <- getBuiltinName (getLoc f) BuiltinNat
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNatSuc
  let plus = f ^. funDefName
      ty = f ^. funDefTypeSig
  unless (ty === (nat --> nat --> nat)) (error "Nat plus has the wrong type signature")
  registerBuiltin BuiltinNatPlus plus
  varn <- freshVar "n"
  varm <- freshVar "m"
  let n = toExpression varn
      m = toExpression varm
      freeVars = HashSet.fromList [varn, varm]
      a =% b = (a ==% b) freeVars
      (.+.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .+. y = plus @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (zero .+. m, m),
          ((suc @@ n) .+. m, suc @@ (n .+. m))
        ]
      clauses :: [(Expression, Expression)]
      clauses =
        [ (clauseLhsAsExpression c, c ^. clauseBody)
          | c <- toList (f ^. funDefClauses)
        ]
  case zipExactMay exClauses clauses of
    Nothing -> error "Nat plus has the wrong number of clauses"
    Just z -> forM_ z $ \((exLhs, exBody), (lhs, body)) -> do
      unless (exLhs =% lhs) (error "clause lhs does not match")
      unless (exBody =% body) (error $ "clause body does not match " <> ppTrace exBody <> " | " <> ppTrace body)
