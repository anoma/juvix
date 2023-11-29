module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Checker where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Data.InfoTable
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Positivity.Error
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Inference
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Error
import Juvix.Compiler.Pipeline.EntryPoint qualified as E
import Juvix.Prelude hiding (fromEither)

type NegativeTypeParameters = HashSet VarName

type CheckPositivityEffects r =
  Members
    '[ Reader E.EntryPoint,
       Reader InfoTable,
       Error TypeCheckerError,
       Inference,
       State NegativeTypeParameters
     ]
    r

data CheckPositivityArgs = CheckPositivityArgs
  { _checkPositivityArgsInductive :: InductiveDef,
    _checkPositivityArgsConstructorName :: Name,
    _checkPositivityArgsInductiveName :: Name,
    _checkPositivityArgsRecursionLimit :: Int,
    _checkPositivityArgsErrorReference :: Maybe Expression,
    _checkPositivityArgsTypeOfConstructor :: Expression
  }

makeLenses ''CheckPositivityArgs

checkPositivity ::
  forall r.
  (CheckPositivityEffects r) =>
  InductiveDef ->
  Sem r ()
checkPositivity ty = do
  unlessM (asks (^. E.entryPointNoPositivity)) $
    forM_ (ty ^. inductiveConstructors) $ \ctor -> do
      unless (ty ^. inductivePositive) $ do
        numInductives <- HashMap.size <$> asks (^. infoInductives)
        mapM_
          ( \typeOfConstr ->
              checkStrictlyPositiveOccurrences
                ( CheckPositivityArgs
                    { _checkPositivityArgsInductive = ty,
                      _checkPositivityArgsConstructorName =
                        ctor ^. inductiveConstructorName,
                      _checkPositivityArgsInductiveName = ty ^. inductiveName,
                      _checkPositivityArgsRecursionLimit = numInductives,
                      _checkPositivityArgsErrorReference = Nothing,
                      _checkPositivityArgsTypeOfConstructor = typeOfConstr
                    }
                )
          )
          (constructorArgs (ctor ^. inductiveConstructorType))

checkStrictlyPositiveOccurrences ::
  forall r.
  (CheckPositivityEffects r) =>
  CheckPositivityArgs ->
  Sem r ()
checkStrictlyPositiveOccurrences p = do
  typeOfConstr <- strongNormalize (p ^. checkPositivityArgsTypeOfConstructor)
  go False typeOfConstr
  where
    ty = p ^. checkPositivityArgsInductive
    ctorName = p ^. checkPositivityArgsConstructorName
    name = p ^. checkPositivityArgsInductiveName
    recLimit = p ^. checkPositivityArgsRecursionLimit
    ref = p ^. checkPositivityArgsErrorReference

    indName :: Name
    indName = ty ^. inductiveName

    {- The following `go` function determines if there is any negative
     occurence of the symbol `name` in the given expression. The `inside` flag
     used below indicates whether the current search is performed on the left of
     an inner arrow or not.
    -}
    go :: Bool -> Expression -> Sem r ()
    go inside expr = case expr of
      ExpressionApplication tyApp -> goApp tyApp
      ExpressionCase l -> goCase l
      ExpressionFunction (Function l r) -> go True (l ^. paramType) >> go inside r
      ExpressionHole {} -> return ()
      ExpressionInstanceHole {} -> return ()
      ExpressionIden i -> goIden i
      ExpressionLambda l -> goLambda l
      ExpressionLet l -> goLet l
      ExpressionLiteral {} -> return ()
      ExpressionSimpleLambda l -> goSimpleLambda l
      ExpressionUniverse {} -> return ()
      where
        goCase :: Case -> Sem r ()
        goCase l = do
          go inside (l ^. caseExpression)
          mapM_ goCaseBranch (l ^. caseBranches)

        goCaseBranch :: CaseBranch -> Sem r ()
        goCaseBranch b = go inside (b ^. caseBranchExpression)

        goLet :: Let -> Sem r ()
        goLet l = do
          go inside (l ^. letExpression)
          mapM_ goLetClause (l ^. letClauses)

        goLetClause :: LetClause -> Sem r ()
        goLetClause = \case
          LetFunDef f -> goFunctionDef f
          LetMutualBlock b -> goMutualBlockLet b

        goMutualBlockLet :: MutualBlockLet -> Sem r ()
        goMutualBlockLet b = mapM_ goFunctionDef (b ^. mutualLet)

        goFunctionDef :: FunctionDef -> Sem r ()
        goFunctionDef d = do
          go inside (d ^. funDefType)
          go inside (d ^. funDefBody)

        goSimpleLambda :: SimpleLambda -> Sem r ()
        goSimpleLambda (SimpleLambda (SimpleBinder _ lamVarTy) lamBody) = do
          go inside lamVarTy
          go inside lamBody

        goLambda :: Lambda -> Sem r ()
        goLambda l = mapM_ goClause (l ^. lambdaClauses)
          where
            goClause :: LambdaClause -> Sem r ()
            goClause (LambdaClause _ b) = go inside b

        goIden :: Iden -> Sem r ()
        goIden = \case
          IdenInductive ty' ->
            when (inside && name == ty') (strictlyPositivityError expr)
          IdenVar name'
            | not inside -> return ()
            | name == name' -> strictlyPositivityError expr
            | name' `elem` ty ^.. inductiveParameters . each . inductiveParamName -> modify (HashSet.insert name')
          _ -> return ()

        goApp :: Application -> Sem r ()
        goApp tyApp = do
          let (hdExpr, args) = unfoldApplication tyApp
          case hdExpr of
            ExpressionIden (IdenInductive ty') -> do
              when (inside && name == ty') (strictlyPositivityError expr)
              InductiveInfo indType' <- lookupInductive ty'

              {- We now need to know whether `name` negatively occurs at
               `indTy'` or not. The way to know is by checking that the type ty'
               preserves the positivity condition, i.e., its type parameters are
               no negative.
              -}
              let paramsTy' = indType' ^. inductiveParameters
              goInductiveApp indType' (zip paramsTy' (toList args))
            _ -> return ()

        goInductiveApp :: InductiveDef -> [(InductiveParameter, Expression)] -> Sem r ()
        goInductiveApp indType' = \case
          [] -> return ()
          (InductiveParameter pName' _ty', tyArg) : ps -> do
            negParms :: NegativeTypeParameters <- get
            when (varOrInductiveInExpression name tyArg) $ do
              when (HashSet.member pName' negParms) (strictlyPositivityError tyArg)
              when (recLimit > 0) $
                forM_ (indType' ^. inductiveConstructors) $ \ctor' -> do
                  let ctorName' = ctor' ^. inductiveConstructorName
                      errorRef = fromMaybe tyArg ref
                      args = constructorArgs (ctor' ^. inductiveConstructorType)
                  mapM_
                    ( \tyConstr' ->
                        checkStrictlyPositiveOccurrences
                          CheckPositivityArgs
                            { _checkPositivityArgsInductive = indType',
                              _checkPositivityArgsConstructorName = ctorName',
                              _checkPositivityArgsInductiveName = pName',
                              _checkPositivityArgsRecursionLimit = recLimit - 1,
                              _checkPositivityArgsErrorReference = Just errorRef,
                              _checkPositivityArgsTypeOfConstructor = tyConstr'
                            }
                    )
                    args
            goInductiveApp indType' ps

    strictlyPositivityError :: Expression -> Sem r ()
    strictlyPositivityError expr = do
      let errLoc = fromMaybe expr ref
      throw
        . ErrNonStrictlyPositive
        . ErrTypeInNegativePosition
        $ TypeInNegativePosition
          { _typeInNegativePositionType = indName,
            _typeInNegativePositionConstructor = ctorName,
            _typeInNegativePositionArgument = errLoc
          }

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
