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

mkBuiltinIden :: (IsBuiltin a, Members '[Reader BuiltinsTable, Error TypeCheckerError] r) => (Name -> Iden) -> Interval -> a -> Sem r Expression
mkBuiltinIden mkIden loc = fmap (ExpressionIden . mkIden) . getBuiltinNameTypeChecker loc

mkBuiltinConstructor :: (Members '[Reader BuiltinsTable, Error TypeCheckerError] r) => Interval -> BuiltinConstructor -> Sem r Expression
mkBuiltinConstructor = mkBuiltinIden IdenConstructor

mkBuiltinInductive :: (Members '[Reader BuiltinsTable, Error TypeCheckerError] r) => Interval -> BuiltinInductive -> Sem r Expression
mkBuiltinInductive = mkBuiltinIden IdenInductive

getIntTy :: (Members '[Reader BuiltinsTable, Error TypeCheckerError] r) => Interval -> Sem r Expression
getIntTy loc = mkBuiltinInductive loc BuiltinInt

getNatTy :: (Members '[Reader BuiltinsTable, Error TypeCheckerError] r) => Interval -> Sem r Expression
getNatTy loc = mkBuiltinInductive loc BuiltinNat

getBuiltinNameScoper ::
  (IsBuiltin a, Members '[Error ScoperError, Reader BuiltinsTable] r) =>
  Interval ->
  a ->
  Sem r Name
getBuiltinNameScoper loc =
  mapError ErrBuiltinNotDefined
    . getBuiltinName loc

matchBuiltinName ::
  (IsBuiltin a, Members '[Reader BuiltinsTable, Fail] r) =>
  a ->
  Name ->
  Sem r ()
matchBuiltinName b m = do
  n <- peekBuiltinNameTypeChecker impossible b
  failUnless (Just m == n)

peekBuiltinNameTypeChecker :: (IsBuiltin a, Members '[Reader BuiltinsTable] r) => Interval -> a -> Sem r (Maybe Name)
peekBuiltinNameTypeChecker loc =
  fmap (either (const Nothing) Just)
    . runError @BuiltinNotDefined
    . getBuiltinName loc

getBuiltinNameTypeChecker :: (IsBuiltin a, Members '[Reader BuiltinsTable, Error TypeCheckerError] r) => Interval -> a -> Sem r Name
getBuiltinNameTypeChecker loc =
  mapError TypeChecker.ErrBuiltinNotDefined
    . getBuiltinName loc

builtinNaturalFromApp ::
  (Members '[Fail, Reader BuiltinsTable] r) =>
  Application ->
  Sem r BuiltinNatural
builtinNaturalFromApp topApp = subsume (failAlts [fromLit, fromSuc])
  where
    -- suc (.. (suc arg))
    fromSuc :: forall r'. (Members '[Reader BuiltinsTable] r') => Sem (Fail ': r') BuiltinNatural
    fromSuc = goSuc 0 (ExpressionApplication topApp)
      where
        goSuc :: Natural -> Expression -> Sem (Fail ': r') BuiltinNatural
        goSuc succs = \case
          ExpressionApplication app -> do
            (ExpressionIden (IdenConstructor suc_), [ApplicationArg Explicit arg_]) <- return (second toList (unfoldApplication' app))
            matchBuiltinName BuiltinNatSuc suc_
            goSuc (1 + succs) arg_
          arg
            | succs == 0 -> fail
            | otherwise ->
                return
                  BuiltinNatural
                    { _builtinNaturalSuc = succs,
                      _builtinNaturalArg = arg,
                      _builtinNaturalLoc = getLoc topApp
                    }

    -- fromNatural {Nat} {{FromNatNatI}} <literal>
    -- NOTE: we do not check that FromNatNatI instance uses the builtin fromNat
    fromLit :: (Members '[Reader BuiltinsTable] r') => Sem (Fail ': r') BuiltinNatural
    fromLit = do
      (ExpressionIden (IdenFunction fromNatural_), [nat_, _fromNatI_, arg_]) <- return (second toList (unfoldApplication' topApp))
      matchBuiltinName BuiltinFromNat fromNatural_
      ApplicationArg Implicit (ExpressionIden (IdenInductive natTy)) <- return nat_
      matchBuiltinName BuiltinNat natTy
      ApplicationArg Explicit (ExpressionLiteral (WithLoc loc (LitNatural num))) <- return arg_
      zero <- runErrorWith @TypeCheckerError (const impossible) (getBuiltinNameTypeChecker loc BuiltinNatZero)
      return
        $ BuiltinNatural
          { _builtinNaturalSuc = num,
            _builtinNaturalLoc = loc,
            _builtinNaturalArg = toExpression zero
          }

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
