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
  { _checkPositivityArgsInductive :: InductiveInfo,
    _checkPositivityArgsConstructorName :: Name,
    _checkPositivityArgsInductiveName :: Name,
    _checkPositivityArgsRecursionLimit :: Int,
    _checkPositivityArgsErrorReference :: Maybe Expression,
    _checkPositivityArgsTypeOfConstructorArg :: Expression
  }

makeLenses ''CheckPositivityArgs

checkPositivity ::
  forall r.
  (CheckPositivityEffects r) =>
  InductiveInfo ->
  Sem r ()
checkPositivity indInfo = do
  unlessM (asks (^. E.entryPointNoPositivity)) $
    forM_ (indInfo ^. inductiveInfoConstructors) $ \ctorName -> do
      ctor <- asks (fromJust . HashMap.lookup ctorName . (^. infoConstructors))
      unless (indInfo ^. inductiveInfoPositive) $ do
        numInductives <- length <$> asks (^. infoInductives)
        forM_
          (constructorArgs (ctor ^. constructorInfoType))
          $ \typeOfConstr ->
            checkStrictlyPositiveOccurrences
              CheckPositivityArgs
                { _checkPositivityArgsInductive = indInfo,
                  _checkPositivityArgsConstructorName = ctorName,
                  _checkPositivityArgsInductiveName = indInfo ^. inductiveInfoName,
                  _checkPositivityArgsRecursionLimit = numInductives,
                  _checkPositivityArgsErrorReference = Nothing,
                  _checkPositivityArgsTypeOfConstructorArg = typeOfConstr ^. paramType
                }

checkStrictlyPositiveOccurrences ::
  forall r.
  (CheckPositivityEffects r) =>
  CheckPositivityArgs ->
  Sem r ()
checkStrictlyPositiveOccurrences p = do
  typeOfConstrArg <- strongNormalize_ (p ^. checkPositivityArgsTypeOfConstructorArg)
  goExpression False typeOfConstrArg
  where
    indInfo = p ^. checkPositivityArgsInductive
    ctorName = p ^. checkPositivityArgsConstructorName
    name = p ^. checkPositivityArgsInductiveName
    recLimit = p ^. checkPositivityArgsRecursionLimit
    ref = p ^. checkPositivityArgsErrorReference

    indName :: Name
    indName = indInfo ^. inductiveInfoName

    {- The following `go` function determines if there is any negative
     occurrence of the symbol `name` in the given expression. The `inside` flag
     used below indicates whether the current search is performed on the left of
     an inner arrow or not.
    -}
    goExpression :: Bool -> Expression -> Sem r ()
    goExpression inside expr = case expr of
      ExpressionApplication tyApp -> goApp tyApp
      ExpressionCase l -> goCase l
      ExpressionFunction (Function l r) -> goLeft (l ^. paramType) >> go r
      ExpressionHole {} -> return ()
      ExpressionInstanceHole {} -> return ()
      ExpressionIden i -> goIden i
      ExpressionLambda l -> goLambda l
      ExpressionLet l -> goLet l
      ExpressionLiteral {} -> return ()
      ExpressionSimpleLambda l -> goSimpleLambda l
      ExpressionUniverse {} -> return ()
      where
        go :: Expression -> Sem r ()
        go = goExpression inside

        goLeft :: Expression -> Sem r ()
        goLeft = goExpression True

        goCase :: Case -> Sem r ()
        goCase l = do
          go (l ^. caseExpression)
          mapM_ goCaseBranch (l ^. caseBranches)

        goSideIfBranch :: SideIfBranch -> Sem r ()
        goSideIfBranch b = do
          go (b ^. sideIfBranchCondition)
          go (b ^. sideIfBranchBody)

        goSideIfs :: SideIfs -> Sem r ()
        goSideIfs s = do
          mapM_ goSideIfBranch (s ^. sideIfBranches)
          mapM_ go (s ^. sideIfElse)

        goCaseBranchRhs :: CaseBranchRhs -> Sem r ()
        goCaseBranchRhs = \case
          CaseBranchRhsExpression e -> go e
          CaseBranchRhsIf s -> goSideIfs s

        goCaseBranch :: CaseBranch -> Sem r ()
        goCaseBranch b = goCaseBranchRhs (b ^. caseBranchRhs)

        goLet :: Let -> Sem r ()
        goLet l = do
          go (l ^. letExpression)
          mapM_ goLetClause (l ^. letClauses)

        goLetClause :: LetClause -> Sem r ()
        goLetClause = \case
          LetFunDef f -> goFunctionDef f
          LetMutualBlock b -> goMutualBlockLet b

        goMutualBlockLet :: MutualBlockLet -> Sem r ()
        goMutualBlockLet b = mapM_ goFunctionDef (b ^. mutualLet)

        goFunctionDef :: FunctionDef -> Sem r ()
        goFunctionDef d = do
          go (d ^. funDefType)
          go (d ^. funDefBody)

        goSimpleLambda :: SimpleLambda -> Sem r ()
        goSimpleLambda (SimpleLambda (SimpleBinder _ lamVarTy) lamBody) = do
          go lamVarTy
          go lamBody

        goLambda :: Lambda -> Sem r ()
        goLambda l = mapM_ goClause (l ^. lambdaClauses)
          where
            goClause :: LambdaClause -> Sem r ()
            goClause (LambdaClause _ b) = go b

        goIden :: Iden -> Sem r ()
        goIden = \case
          IdenInductive ty' ->
            when (inside && name == ty') (throwNegativePositonError expr)
          IdenVar name'
            | not inside -> return ()
            | name == name' -> throwNegativePositonError expr
            | name' `elem` indInfo ^.. inductiveInfoParameters . each . inductiveParamName -> modify (HashSet.insert name')
            | otherwise -> return ()
          _ -> return ()

        goApp :: Application -> Sem r ()
        goApp tyApp = do
          let (hdExpr, args) = unfoldApplication tyApp
          case hdExpr of
            ax@(ExpressionIden IdenAxiom {}) -> do
              when (isJust $ find (varOrInductiveInExpression name) args) $
                throwTypeAsArgumentOfBoundVarError ax
            var@(ExpressionIden IdenVar {}) -> do
              when (isJust $ find (varOrInductiveInExpression name) args) $
                throwTypeAsArgumentOfBoundVarError var
            ExpressionIden (IdenInductive ty') -> do
              when (inside && name == ty') (throwNegativePositonError expr)
              indInfo' <- lookupInductive ty'
              {- We now need to know whether `name` negatively occurs at
               `indTy'` or not. The way to know is by checking that the type ty'
               preserves the positivity condition, i.e., its type parameters are
               no negative.
              -}
              let paramsTy' = indInfo' ^. inductiveInfoParameters
              goInductiveApp indInfo' (zipExact paramsTy' (toList args))
            _ -> return ()

        goInductiveApp :: InductiveInfo -> [(InductiveParameter, Expression)] -> Sem r ()
        goInductiveApp indInfo' = \case
          [] -> return ()
          (InductiveParameter pName' _ty', tyArg) : ps -> do
            negParms :: NegativeTypeParameters <- get
            when (varOrInductiveInExpression name tyArg) $ do
              when
                (HashSet.member pName' negParms)
                (throwNegativePositonError tyArg)
              when (recLimit > 0) $
                forM_ (indInfo' ^. inductiveInfoConstructors) $ \ctorName' -> do
                  ctorType' <- lookupConstructorType ctorName'
                  let errorRef = fromMaybe tyArg ref
                      args = constructorArgs ctorType'
                  mapM_
                    ( \tyConstr' ->
                        checkStrictlyPositiveOccurrences
                          CheckPositivityArgs
                            { _checkPositivityArgsInductive = indInfo',
                              _checkPositivityArgsConstructorName = ctorName',
                              _checkPositivityArgsInductiveName = pName',
                              _checkPositivityArgsRecursionLimit = recLimit - 1,
                              _checkPositivityArgsErrorReference = Just errorRef,
                              _checkPositivityArgsTypeOfConstructorArg = tyConstr' ^. paramType
                            }
                    )
                    args
            goInductiveApp indInfo' ps

    throwNegativePositonError :: Expression -> Sem r ()
    throwNegativePositonError expr = do
      let errLoc = fromMaybe expr ref
      throw
        . ErrNonStrictlyPositive
        . ErrTypeInNegativePosition
        $ TypeInNegativePosition
          { _typeInNegativePositionType = indName,
            _typeInNegativePositionConstructor = ctorName,
            _typeInNegativePositionArgument = errLoc
          }

    throwTypeAsArgumentOfBoundVarError :: Expression -> Sem r ()
    throwTypeAsArgumentOfBoundVarError expr = do
      let errLoc = fromMaybe expr ref
      throw
        . ErrNonStrictlyPositive
        . ErrTypeAsArgumentOfBoundVar
        $ TypeAsArgumentOfBoundVar
          { _typeAsArgumentOfBoundVarType = indName,
            _typeAsArgumentOfBoundVarConstructor = ctorName,
            _typeAsArgumentOfBoundVarReference = errLoc
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
