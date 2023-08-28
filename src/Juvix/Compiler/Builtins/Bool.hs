module Juvix.Compiler.Builtins.Bool where

import Juvix.Compiler.Builtins.Effect
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Prelude

registerBoolDef :: (Member Builtins r) => InductiveDef -> Sem r ()
registerBoolDef d = do
  unless (null (d ^. inductiveParameters)) (error "Bool should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (error "Bool should be in the small universe")
  registerBuiltin BuiltinBool (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c1, c2] -> registerTrue c1 >> registerFalse c2
    _ -> error "Bool should have exactly two constructors"

registerTrue :: (Member Builtins r) => ConstructorDef -> Sem r ()
registerTrue d@ConstructorDef {..} = do
  let ctorTrue = _inductiveConstructorName
      ctorTy = _inductiveConstructorType
  boolTy <- getBuiltinName (getLoc d) BuiltinBool
  unless (ctorTy === boolTy) (error $ "true has the wrong type " <> ppTrace ctorTy <> " | " <> ppTrace boolTy)
  registerBuiltin BuiltinBoolTrue ctorTrue

registerFalse :: (Member Builtins r) => ConstructorDef -> Sem r ()
registerFalse d@ConstructorDef {..} = do
  let ctorFalse = _inductiveConstructorName
      ctorTy = _inductiveConstructorType
  boolTy <- getBuiltinName (getLoc d) BuiltinBool
  unless (ctorTy === boolTy) (error $ "false has the wrong type " <> ppTrace ctorTy <> " | " <> ppTrace boolTy)
  registerBuiltin BuiltinBoolFalse ctorFalse

registerIf :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerIf f = do
  bool_ <- getBuiltinName (getLoc f) BuiltinBool
  true_ <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolTrue
  false_ <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolFalse
  let if_ = f ^. funDefName
      u = ExpressionUniverse smallUniverseNoLoc
      l = getLoc f
  vart <- freshVar l "t"
  vare <- freshVar l "e"
  hole <- freshHole l
  let e = toExpression vare
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (if_ @@ true_ @@ e @@ hole, e),
          (if_ @@ false_ @@ hole @@ e, e)
        ]
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinBoolIf,
        _funInfoSignature = u <>--> bool_ --> vart --> vart --> vart,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [vare],
        _funInfoFreeTypeVars = [vart]
      }

registerOr :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerOr f = do
  bool_ <- getBuiltinName (getLoc f) BuiltinBool
  true_ <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolTrue
  false_ <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolFalse
  let or_ = f ^. funDefName
      l = getLoc f
  vare <- freshVar l "e"
  hole <- freshHole l
  let e = toExpression vare
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (or_ @@ true_ @@ hole, true_),
          (or_ @@ false_ @@ e, e)
        ]
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinBoolOr,
        _funInfoSignature = bool_ --> bool_ --> bool_,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [vare],
        _funInfoFreeTypeVars = []
      }

registerAnd :: (Members '[Builtins, NameIdGen] r) => FunctionDef -> Sem r ()
registerAnd f = do
  bool_ <- getBuiltinName (getLoc f) BuiltinBool
  true_ <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolTrue
  false_ <- toExpression <$> getBuiltinName (getLoc f) BuiltinBoolFalse
  let and_ = f ^. funDefName
      l = getLoc f
  vare <- freshVar l "e"
  hole <- freshHole l
  let e = toExpression vare
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (and_ @@ true_ @@ e, e),
          (and_ @@ false_ @@ hole, false_)
        ]
  registerFun
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinBoolAnd,
        _funInfoSignature = bool_ --> bool_ --> bool_,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [vare],
        _funInfoFreeTypeVars = []
      }

registerBoolPrint :: (Members '[Builtins] r) => AxiomDef -> Sem r ()
registerBoolPrint f = do
  bool_ <- getBuiltinName (getLoc f) BuiltinBool
  io <- getBuiltinName (getLoc f) BuiltinIO
  unless (f ^. axiomType === (bool_ --> io)) (error "Bool print has the wrong type signature")
  registerBuiltin BuiltinBoolPrint (f ^. axiomName)
