module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.ConstructorArg
  ( mkConstructorArg,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.ConstructorArg.Base,
  )
where

import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.ConstructorArg.Base
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Prelude

mkConstructorArg :: (Members '[Error TypeCheckerError] r) => NormalizedExpression -> Sem r [ConstructorArg]
mkConstructorArg = mapM (goArg . (^. paramType)) . fst . unfoldFunType . (^. normalizedExpression)

goArg :: forall r. (Members '[Error TypeCheckerError] r) => Expression -> Sem r ConstructorArg
goArg ty = case ty of
  ExpressionIden i -> goApplicationHelper (ExpressionIden i, [])
  ExpressionApplication i -> goApplication i
  ExpressionUniverse {} -> return ConstructorArgType
  ExpressionFunction i -> goFunction i
  ExpressionLiteral {} -> invalid
  ExpressionHole {} -> invalid
  ExpressionInstanceHole {} -> invalid
  ExpressionLet {} -> invalid
  ExpressionSimpleLambda {} -> invalid
  ExpressionLambda {} -> invalid
  ExpressionCase {} -> invalid
  ExpressionNatural {} -> invalid
  where
    invalid :: forall a. Sem r a
    invalid =
      throw
        $ ErrInvalidConstructorArgType
          InvalidConstructorArgType
            { _invalidConstructorArgType = ty
            }

    getIden :: Expression -> Sem r Iden
    getIden = \case
      ExpressionIden i -> return i
      _ -> invalid

    goFunction :: Function -> Sem r ConstructorArg
    goFunction fun = do
      l <- goArg (fun ^. functionLeft . paramType)
      r <- goArg (fun ^. functionRight)
      return
        $ ConstructorArgFun
          Fun
            { _funLeft = l,
              _funRight = r
            }

    goApplication :: Application -> Sem r ConstructorArg
    goApplication = goApplicationHelper . second toList . unfoldApplication

    goApplicationHelper :: (Expression, [Expression]) -> Sem r ConstructorArg
    goApplicationHelper (f, args) = do
      i <- getIden f
      cargs <- mapM goArg args
      lhs :: AppLhs <- case i of
        IdenFunction {} -> invalid
        IdenConstructor {} -> invalid
        IdenVar v -> return (AppVar v)
        IdenAxiom v -> return (AppAxiom v)
        IdenInductive v -> return (AppInductive v)
      return (ConstructorArgApp (App lhs cargs))
