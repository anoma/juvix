module Juvix.Compiler.Builtins.Boolean where

import Data.HashSet qualified as HashSet
import Juvix.Compiler.Abstract.Extra
import Juvix.Compiler.Abstract.Pretty
import Juvix.Compiler.Builtins.Effect
import Juvix.Prelude

registerBoolDef :: Member Builtins r => InductiveDef -> Sem r ()
registerBoolDef d = do
  unless (null (d ^. inductiveParameters)) (error "Bool should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (error "Bool should be in the small universe")
  registerBuiltin BuiltinBoolean (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c1, c2] -> registerTrue c1 >> registerFalse c2
    _ -> error "Bool should have exactly two constructors"

registerTrue :: Member Builtins r => InductiveConstructorDef -> Sem r ()
registerTrue d@InductiveConstructorDef {..} = do
  let ctorTrue = _constructorName
      ctorTy = _constructorType
  boolTy <- getBuiltinName (getLoc d) BuiltinBoolean
  unless (ctorTy === boolTy) (error $ "true has the wrong type " <> ppTrace ctorTy <> " | " <> ppTrace boolTy)
  registerBuiltin BuiltinBooleanTrue ctorTrue

registerFalse :: Member Builtins r => InductiveConstructorDef -> Sem r ()
registerFalse d@InductiveConstructorDef {..} = do
  let ctorFalse = _constructorName
      ctorTy = _constructorType
  boolTy <- getBuiltinName (getLoc d) BuiltinBoolean
  unless (ctorTy === boolTy) (error $ "false has the wrong type " <> ppTrace ctorTy <> " | " <> ppTrace boolTy)
  registerBuiltin BuiltinBooleanFalse ctorFalse

registerIf :: Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerIf f = do
  boolean <- getBuiltinName (getLoc f) BuiltinBoolean
  true_ <- toExpression <$> getBuiltinName (getLoc f) BuiltinBooleanTrue
  false_ <- toExpression <$> getBuiltinName (getLoc f) BuiltinBooleanFalse
  vart <- freshVar "t"
  let if_ = f ^. funDefName
      ty = f ^. funDefTypeSig
      freeTVars = HashSet.fromList [vart]
      u = ExpressionUniverse (Universe {_universeLevel = Nothing, _universeLoc = error "Universe with no location"})
  unless (((u <>--> boolean --> vart --> vart --> vart) ==% ty) freeTVars) (error "Boolean if has the wrong type signature")
  registerBuiltin BuiltinBooleanIf if_
  vare <- freshVar "e"
  hole <- freshHole
  let e = toExpression vare
      freeVars = HashSet.fromList [vare]
      (=%) :: (IsExpression a, IsExpression b) => a -> b -> Bool
      a =% b = (a ==% b) freeVars
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (if_ @@ true_ @@ e @@ hole, e),
          (if_ @@ false_ @@ hole @@ e, e)
        ]
      clauses :: [(Expression, Expression)]
      clauses =
        [ (clauseLhsAsExpression c, c ^. clauseBody)
          | c <- toList (f ^. funDefClauses)
        ]
  case zipExactMay exClauses clauses of
    Nothing -> error "Boolean if has the wrong number of clauses"
    Just z -> forM_ z $ \((exLhs, exBody), (lhs, body)) -> do
      unless (exLhs =% lhs) (error "clause lhs does not match")
      unless (exBody =% body) (error $ "clause body does not match " <> ppTrace exBody <> " | " <> ppTrace body)
