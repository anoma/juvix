module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.ConstructorArg where

import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data ConstructorArg
  = ConstructorArgFun Fun
  | ConstructorArgApp App

data Fun = Fun
  { _funLeft :: ConstructorArg,
    _funRight :: ConstructorArg
  }

data AppLhs
  = AppVar VarName
  | AppAxiom AxiomName
  | AppInductive InductiveName
  deriving stock (Eq, Generic)

instance Hashable AppLhs

data App = App
  { _appLhs :: AppLhs,
    _appArgs :: [ConstructorArg]
  }

-- | The
mkConstructorArg :: (Members '[Error ()] r) => NormalizedExpression -> Sem r ConstructorArg
mkConstructorArg = mkConstructorArg' . (^. normalizedExpression)

mkConstructorArg' :: forall r. (Members '[Error ()] r) => Expression -> Sem r ConstructorArg
mkConstructorArg' = \case
  ExpressionIden i -> goApplicationHelper (ExpressionIden i, [])
  ExpressionApplication i -> goApplication i
  ExpressionFunction i -> goFunction i
  ExpressionLiteral {} -> throw ()
  ExpressionHole {} -> throw ()
  ExpressionInstanceHole {} -> throw ()
  ExpressionLet {} -> throw ()
  ExpressionUniverse {} -> throw ()
  ExpressionSimpleLambda {} -> throw ()
  ExpressionLambda {} -> throw ()
  ExpressionCase {} -> throw ()
  where
    getIden :: Expression -> Sem r Iden
    getIden = \case
      ExpressionIden i -> return i
      _ -> throw ()

    goFunction :: Function -> Sem r ConstructorArg
    goFunction fun = do
      l <- mkConstructorArg' (fun ^. functionLeft . paramType)
      r <- mkConstructorArg' (fun ^. functionRight)
      return $
        ConstructorArgFun
          Fun
            { _funLeft = l,
              _funRight = r
            }

    goApplication :: Application -> Sem r ConstructorArg
    goApplication = goApplicationHelper . second toList . unfoldApplication

    goApplicationHelper :: (Expression, [Expression]) -> Sem r ConstructorArg
    goApplicationHelper (f, args) = do
      i <- getIden f
      cargs <- mapM mkConstructorArg' args
      lhs :: AppLhs <- case i of
        IdenFunction {} -> throw ()
        IdenConstructor {} -> throw ()
        IdenVar v -> return (AppVar v)
        IdenAxiom v -> return (AppAxiom v)
        IdenInductive v -> return (AppInductive v)
      return (ConstructorArgApp (App lhs cargs))
