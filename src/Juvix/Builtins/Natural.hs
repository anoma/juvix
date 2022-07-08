module Juvix.Builtins.Natural where

import Data.HashSet qualified as HashSet
import Juvix.Builtins.Effect
import Juvix.Internal.NameIdGen
import Juvix.Prelude
import Juvix.Syntax.Abstract.Language.Extra
import Juvix.Syntax.Abstract.Pretty

registerNaturalDef :: Member Builtins r => InductiveDef -> Sem r ()
registerNaturalDef d = do
  unless (null (d ^. inductiveParameters)) (error "Naturals should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (error "Naturals should be in the small universe")
  registerBuiltin BuiltinNatural (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c1, c2] -> registerZero c1 >> registerSuc c2
    _ -> error "Natural numbers should have exactly two constructors"

registerZero :: Member Builtins r => InductiveConstructorDef -> Sem r ()
registerZero d@(InductiveConstructorDef zero ty) = do
  nat <- getBuiltinName (getLoc d) BuiltinNatural
  unless (ty === nat) (error $ "zero has the wrong type " <> ppSimple ty <> " | " <> ppSimple nat)
  registerBuiltin BuiltinNaturalZero zero

registerSuc :: Member Builtins r => InductiveConstructorDef -> Sem r ()
registerSuc d@(InductiveConstructorDef suc ty) = do
  nat <- getBuiltinName (getLoc d) BuiltinNatural
  unless (ty === (nat --> nat)) (error "suc has the wrong type")
  registerBuiltin BuiltinNaturalSuc suc

registerNaturalPrint :: Members '[Builtins] r => AxiomDef -> Sem r ()
registerNaturalPrint f = do
  nat <- getBuiltinName (getLoc f) BuiltinNatural
  io <- getBuiltinName (getLoc f) BuiltinIO
  unless (f ^. axiomType === (nat --> io)) (error "Natural print has the wrong type signature")
  registerBuiltin BuiltinNaturalPrint (f ^. axiomName)

registerNaturalPlus :: Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerNaturalPlus f = do
  nat <- getBuiltinName (getLoc f) BuiltinNatural
  zero <- toExpression <$> getBuiltinName (getLoc f) BuiltinNaturalZero
  suc <- toExpression <$> getBuiltinName (getLoc f) BuiltinNaturalSuc
  let plus = f ^. funDefName
      ty = f ^. funDefTypeSig
  unless (ty === (nat --> nat --> nat)) (error "Natural plus has the wrong type signature")
  registerBuiltin BuiltinNaturalPlus plus
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
    Nothing -> error "Natural plus has the wrong number of clauses"
    Just z -> forM_ z $ \((exLhs, exBody), (lhs, body)) -> do
      unless (exLhs =% lhs) (error "clause lhs does not match")
      unless (exBody =% body) (error $ "clause body does not match " <> ppSimple exBody <> " | " <> ppSimple body)
