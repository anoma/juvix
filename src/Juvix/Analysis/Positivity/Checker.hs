-- | Checker for strictly positive inductive data types
module Juvix.Analysis.Positivity.Checker where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Pipeline.EntryPoint qualified as E
import Juvix.Prelude hiding (fromEither)
import Juvix.Syntax.MicroJuvix.Error
import Juvix.Syntax.MicroJuvix.InfoTable
import Juvix.Syntax.MicroJuvix.Language.Extra
import Juvix.Syntax.MicroJuvix.TypeChecker.Inference

type NegativeTypeParameters = HashSet VarName

type ErrorReference = Maybe Expression

type RecursionLimit = Int

checkPositivity ::
  Members
    '[ Reader E.EntryPoint,
       Reader InfoTable,
       Error TypeCheckerError,
       Inference,
       State NegativeTypeParameters
     ]
    r =>
  InductiveDef ->
  Sem r ()
checkPositivity ty = do
  let indName = ty ^. inductiveName
  numInductives <- HashMap.size <$> asks (^. infoInductives)
  noCheckPositivity <- asks (^. E.entryPointNoPositivity)
  forM_ (ty ^. inductiveConstructors) $ \ctor -> do
    let ctorName = ctor ^. inductiveConstructorName
    unless (noCheckPositivity || ty ^. inductivePositive) $
      mapM_
        (checkStrictlyPositiveOccurrences ty ctorName indName numInductives Nothing)
        (ctor ^. inductiveConstructorParameters)

checkStrictlyPositiveOccurrences ::
  forall r.
  Members '[Reader InfoTable, Error TypeCheckerError, State NegativeTypeParameters, Inference] r =>
  InductiveDef ->
  ConstrName ->
  Name ->
  RecursionLimit ->
  ErrorReference ->
  Expression ->
  Sem r ()
checkStrictlyPositiveOccurrences ty ctorName name recLimit ref =
  strongNormalize >=> helper False
  where
    indName :: Name
    indName = ty ^. inductiveName

    -- The following `helper` function determines if there is any negative
    -- occurence of the symbol `name` in the given expression. The `inside` flag
    -- used below indicates whether the current search is performed on the left
    -- of an inner arrow or not.

    helper :: Bool -> Expression -> Sem r ()
    helper inside expr = case expr of
      ExpressionIden i -> helperIden i
      ExpressionFunction (Function l r) -> helper True (l ^. paramType) >> helper inside r
      ExpressionApplication tyApp -> helperApp tyApp
      ExpressionLiteral {} -> return ()
      ExpressionHole {} -> return ()
      ExpressionUniverse {} -> return ()
      ExpressionLambda l -> helperLambda l
      where
        helperLambda :: Lambda -> Sem r ()
        helperLambda (Lambda _ lamVarTy lamBody) = do
          helper inside lamVarTy
          helper inside lamBody

        helperIden :: Iden -> Sem r ()
        helperIden = \case
          IdenInductive ty' -> when (inside && name == ty') (strictlyPositivityError expr)
          IdenVar name'
            | not inside -> return ()
            | name == name' -> strictlyPositivityError expr
            | InductiveParameter name' `elem` ty ^. inductiveParameters -> modify (HashSet.insert name')
            | otherwise -> return ()
          _ -> return ()

        helperApp :: Application -> Sem r ()
        helperApp tyApp = do
          let (hdExpr, args) = unfoldApplication tyApp
          case hdExpr of
            ExpressionIden (IdenInductive ty') -> do
              when (inside && name == ty') (strictlyPositivityError expr)
              InductiveInfo indTy' <- lookupInductive ty'

              -- We now need to know whether `name` negatively occurs at `indTy'`
              -- or not. The way to know is by checking that the type ty'
              -- preserves the positivity condition, i.e., its type parameters
              -- are no negative.

              let paramsTy' = indTy' ^. inductiveParameters
              helperInductiveApp indTy' (zip paramsTy' (toList args))
            _ -> return ()

        helperInductiveApp :: InductiveDef -> [(InductiveParameter, Expression)] -> Sem r ()
        helperInductiveApp typ = \case
          ((InductiveParameter pName, arg) : ps) -> do
            negParms :: NegativeTypeParameters <- get
            when (varOrInductiveInExpression name arg) $ do
              when (HashSet.member pName negParms) (strictlyPositivityError arg)
              when (recLimit > 0) $
                forM_ (typ ^. inductiveConstructors) $ \ctor' ->
                  mapM_
                    ( checkStrictlyPositiveOccurrences
                        ty
                        ctorName
                        pName
                        (recLimit - 1)
                        (Just (fromMaybe arg ref))
                    )
                    (ctor' ^. inductiveConstructorParameters)
                    >> modify (HashSet.insert pName)
            helperInductiveApp ty ps
          [] -> return ()

    strictlyPositivityError :: Expression -> Sem r ()
    strictlyPositivityError expr = do
      let errLoc = fromMaybe expr ref
      throw
        ( ErrNoPositivity $
            NoPositivity
              { _noStrictPositivityType = indName,
                _noStrictPositivityConstructor = ctorName,
                _noStrictPositivityArgument = errLoc
              }
        )

varOrInductiveInExpression :: Name -> Expression -> Bool
varOrInductiveInExpression n = \case
  ExpressionIden (IdenVar var) -> n == var
  ExpressionIden (IdenInductive ty) -> n == ty
  ExpressionApplication (Application l r _) ->
    varOrInductiveInExpression n l || varOrInductiveInExpression n r
  ExpressionFunction (Function l r) ->
    varOrInductiveInExpression n (l ^. paramType)
      || varOrInductiveInExpression n r
  _ -> False
