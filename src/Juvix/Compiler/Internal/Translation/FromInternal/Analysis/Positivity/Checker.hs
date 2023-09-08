-- | Checker for strictly positive inductive data types
module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Checker where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Inference
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude hiding (fromEither)

type NegativeTypeParameters = HashSet VarName

type ErrorReference = Maybe Expression

type TypeOfConstructor = Expression

type RecursionLimit = Int

type CheckPositivityEffects r =
  Members
    '[ Reader E.EntryPoint,
       Reader InfoTable,
       Error TypeCheckerError,
       Inference,
       State NegativeTypeParameters
     ]
    r

checkPositivity ::
  forall r.
  (CheckPositivityEffects r) =>
  InductiveDef ->
  Sem r ()
checkPositivity ty = do
  let indName = ty ^. inductiveName
  numInductives <- HashMap.size <$> asks (^. infoInductives)
  noCheckPositivity <- asks (^. E.entryPointNoPositivity)
  forM_ (ty ^. inductiveConstructors) $ \ctor -> do
    let ctorName = ctor ^. inductiveConstructorName
        args = constructorArgs (ctor ^. inductiveConstructorType)
    unless (noCheckPositivity || ty ^. inductivePositive) $
      forM_
        args
        (checkStrictlyPositiveOccurrences ty ctorName indName numInductives Nothing)

checkStrictlyPositiveOccurrences ::
  forall r.
  (CheckPositivityEffects r) =>
  InductiveDef ->
  ConstrName ->
  Name ->
  RecursionLimit ->
  ErrorReference ->
  TypeOfConstructor ->
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
      ExpressionApplication tyApp -> helperApp tyApp
      ExpressionCase l -> helperCase l
      ExpressionFunction (Function l r) -> helper True (l ^. paramType) >> helper inside r
      ExpressionHole {} -> return ()
      ExpressionInstanceHole {} -> return ()
      ExpressionIden i -> helperIden i
      ExpressionLambda l -> helperLambda l
      ExpressionLet l -> helperLet l
      ExpressionLiteral {} -> return ()
      ExpressionSimpleLambda l -> helperSimpleLambda l
      ExpressionUniverse {} -> return ()
      where
        helperCase :: Case -> Sem r ()
        helperCase l = do
          helper inside (l ^. caseExpression)
          mapM_ helperCaseBranch (l ^. caseBranches)

        helperCaseBranch :: CaseBranch -> Sem r ()
        helperCaseBranch b = helper inside (b ^. caseBranchExpression)

        helperLet :: Let -> Sem r ()
        helperLet l = do
          helper inside (l ^. letExpression)
          mapM_ helperLetClause (l ^. letClauses)

        helperLetClause :: LetClause -> Sem r ()
        helperLetClause = \case
          LetFunDef f -> helperFunctionDef f
          LetMutualBlock b -> helperMutualBlockLet b

        helperMutualBlockLet :: MutualBlockLet -> Sem r ()
        helperMutualBlockLet b = mapM_ helperFunctionDef (b ^. mutualLet)

        helperFunctionDef :: FunctionDef -> Sem r ()
        helperFunctionDef d = do
          helper inside (d ^. funDefType)
          mapM_ helperFunctionClause (d ^. funDefClauses)

        helperFunctionClause :: FunctionClause -> Sem r ()
        helperFunctionClause c = helper inside (c ^. clauseBody)

        helperSimpleLambda :: SimpleLambda -> Sem r ()
        helperSimpleLambda (SimpleLambda _ lamVarTy lamBody) = do
          helper inside lamVarTy
          helper inside lamBody

        helperLambda :: Lambda -> Sem r ()
        helperLambda l = mapM_ goClause (l ^. lambdaClauses)
          where
            goClause :: LambdaClause -> Sem r ()
            goClause (LambdaClause _ b) = helper inside b

        helperIden :: Iden -> Sem r ()
        helperIden = \case
          IdenInductive ty' ->
            when (inside && name == ty') (strictlyPositivityError expr)
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
              InductiveInfo indType' <- lookupInductive ty'

              -- We now need to know whether `name` negatively occurs at `indTy'`
              -- or not. The way to know is by checking that the type ty'
              -- preserves the positivity condition, i.e., its type parameters
              -- are no negative.

              let paramsTy' = indType' ^. inductiveParameters
              helperInductiveApp indType' (zip paramsTy' (toList args))
            _ -> return ()

        helperInductiveApp :: InductiveDef -> [(InductiveParameter, Expression)] -> Sem r ()
        helperInductiveApp indType' = \case
          ((InductiveParameter pName', tyArg) : ps) -> do
            negParms :: NegativeTypeParameters <- get
            when (varOrInductiveInExpression name tyArg) $ do
              when (HashSet.member pName' negParms) (strictlyPositivityError tyArg)
              when (recLimit > 0) $
                forM_ (indType' ^. inductiveConstructors) $ \ctor' -> do
                  let ctorName' = ctor' ^. inductiveConstructorName
                      errorRef = fromMaybe tyArg ref
                      args = constructorArgs (ctor' ^. inductiveConstructorType)
                  mapM_
                    ( checkStrictlyPositiveOccurrences
                        indType'
                        ctorName'
                        pName'
                        (recLimit - 1)
                        (Just errorRef)
                    )
                    args
            helperInductiveApp indType' ps
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
