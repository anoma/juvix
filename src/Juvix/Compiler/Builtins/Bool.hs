module Juvix.Compiler.Builtins.Bool where

import Juvix.Compiler.Internal.Builtins
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Prelude

checkBoolDef :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => InductiveDef -> Sem r ()
checkBoolDef d = do
  let err = builtinsErrorText (getLoc d)
  unless (null (d ^. inductiveParameters)) (err "Bool should have no type parameters")
  unless (isSmallUniverse' (d ^. inductiveType)) (err "Bool should be in the small universe")
  case d ^. inductiveConstructors of
    [c1, c2] -> checkTrue c1 >> checkFalse c2
    _ -> err "Bool should have exactly two constructors"

checkTrue :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => ConstructorDef -> Sem r ()
checkTrue d@ConstructorDef {..} = do
  let ctorTy = _inductiveConstructorType
  boolTy <- getBuiltinNameScoper (getLoc d) BuiltinBool
  unless (ctorTy === boolTy)
    $ builtinsErrorMsg (getLoc d)
    $ "true has the wrong type "
    <> ppOutDefault ctorTy
    <> " | "
    <> ppOutDefault boolTy

checkFalse :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => ConstructorDef -> Sem r ()
checkFalse d@ConstructorDef {..} = do
  let ctorTy = _inductiveConstructorType
  boolTy <- getBuiltinNameScoper (getLoc d) BuiltinBool
  unless (ctorTy === boolTy)
    $ builtinsErrorMsg (getLoc d)
    $ "false has the wrong type "
    <> ppOutDefault ctorTy
    <> " | "
    <> ppOutDefault boolTy

checkIf :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkIf f = do
  bool_ <- getBuiltinNameScoper (getLoc f) BuiltinBool
  true_ <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinBoolTrue
  false_ <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinBoolFalse
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
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinBoolIf,
        _funInfoSignature = u <>--> bool_ --> vart --> vart --> vart,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [vare],
        _funInfoFreeTypeVars = [vart]
      }

checkOr :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkOr f = do
  bool_ <- getBuiltinNameScoper (getLoc f) BuiltinBool
  true_ <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinBoolTrue
  false_ <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinBoolFalse
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
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinBoolOr,
        _funInfoSignature = bool_ --> bool_ --> bool_,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [vare],
        _funInfoFreeTypeVars = []
      }

checkAnd :: (Members '[Reader BuiltinsTable, Error ScoperError, NameIdGen] r) => FunctionDef -> Sem r ()
checkAnd f = do
  bool_ <- getBuiltinNameScoper (getLoc f) BuiltinBool
  true_ <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinBoolTrue
  false_ <- toExpression <$> getBuiltinNameScoper (getLoc f) BuiltinBoolFalse
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
  checkBuiltinFunctionInfo
    FunInfo
      { _funInfoDef = f,
        _funInfoBuiltin = BuiltinBoolAnd,
        _funInfoSignature = bool_ --> bool_ --> bool_,
        _funInfoClauses = exClauses,
        _funInfoFreeVars = [vare],
        _funInfoFreeTypeVars = []
      }

checkBoolPrint :: (Members '[Reader BuiltinsTable, Error ScoperError] r) => AxiomDef -> Sem r ()
checkBoolPrint f = do
  bool_ <- getBuiltinNameScoper (getLoc f) BuiltinBool
  io <- getBuiltinNameScoper (getLoc f) BuiltinIO
  unless (f ^. axiomType === (bool_ --> io))
    $ builtinsErrorText (getLoc f) "Bool print has the wrong type signature"
