module Juvix.Compiler.Internal.Builtins
  ( module Juvix.Compiler.Internal.Builtins,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error,
    BuiltinsTable,
  )
where

import Juvix.Compiler.Concrete.Data.InfoTableBuilder (getBuiltinSymbolHelper)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error (BuiltinNotDefined (..), ScoperError (ErrBuiltinNotDefined), builtinsErrorMsg, builtinsErrorText)
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error (TypeCheckerError)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error qualified as TypeChecker
import Juvix.Compiler.Store.Scoped.Data.InfoTable (BuiltinsTable)
import Juvix.Prelude

data FunInfo = FunInfo
  { _funInfoDef :: FunctionDef,
    _funInfoBuiltin :: BuiltinFunction,
    _funInfoSignature :: Expression,
    _funInfoClauses :: [(Expression, Expression)],
    _funInfoFreeVars :: [VarName],
    _funInfoFreeTypeVars :: [VarName]
  }

makeLenses ''FunInfo

getBuiltinNameScoper ::
  (IsBuiltin a, Members '[Error ScoperError, Reader BuiltinsTable] r) =>
  Interval ->
  a ->
  Sem r Name
getBuiltinNameScoper loc =
  mapError ErrBuiltinNotDefined
    . getBuiltinName loc

peekBuiltinNameTypeChecker :: (IsBuiltin a, Members '[Reader BuiltinsTable] r) => Interval -> a -> Sem r (Maybe Name)
peekBuiltinNameTypeChecker loc =
  fmap (either (const Nothing) Just)
    . runError @BuiltinNotDefined
    . getBuiltinName loc

getBuiltinNameTypeChecker :: (IsBuiltin a, Members '[Reader BuiltinsTable, Error TypeCheckerError] r) => Interval -> a -> Sem r Name
getBuiltinNameTypeChecker loc =
  mapError TypeChecker.ErrBuiltinNotDefined
    . getBuiltinName loc

getBuiltinName ::
  (IsBuiltin a, Members '[Error BuiltinNotDefined, Reader BuiltinsTable] r) =>
  Interval ->
  a ->
  Sem r Name
getBuiltinName loc p = fromConcreteSymbol loc <$> getBuiltinSymbolHelper loc (toBuiltinPrim p)

checkBuiltinFunctionInfo ::
  forall r.
  (Members '[Error ScoperError] r) =>
  FunInfo ->
  Sem r ()
checkBuiltinFunctionInfo fi = do
  let op = fi ^. funInfoDef . funDefName
      ty = fi ^. funInfoDef . funDefType
      sig = fi ^. funInfoSignature
      err :: forall a. AnsiText -> Sem r a
      err = builtinsErrorMsg (getLoc (fi ^. funInfoDef))
  unless ((sig ==% ty) (hashSet (fi ^. funInfoFreeTypeVars))) (err "builtin has the wrong type signature")
  let freeVars = hashSet (fi ^. funInfoFreeVars)
      a =% b = (a ==% b) freeVars
      clauses :: [(Expression, Expression)]
      clauses =
        [ (clauseLhsAsExpression op (toList pats), body)
          | Just cls <- [unfoldLambdaClauses (fi ^. funInfoDef . funDefBody)],
            (pats, body) <- toList cls
        ]
  case zipExactMay (fi ^. funInfoClauses) clauses of
    Nothing -> err "builtin has the wrong number of clauses"
    Just z -> forM_ z $ \((exLhs, exBody), (lhs, body)) -> do
      unless
        (exLhs =% lhs)
        ( err
            ( "clause lhs does not match for "
                <> ppOutDefault op
                <> "\nExpected: "
                <> ppOutDefault exLhs
                <> "\nActual: "
                <> ppOutDefault lhs
            )
        )
      unless (exBody =% body) (error $ "clause body does not match " <> ppTrace exBody <> " | " <> ppTrace body)
