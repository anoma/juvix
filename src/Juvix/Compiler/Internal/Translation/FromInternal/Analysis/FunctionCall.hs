module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.FunctionCall
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.FunctionCall,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data
import Juvix.Prelude

-- type FunCall = FunCall' Expression
-- type CallMap = CallMap' Expression
-- type FunCallArg = FunCallArg' Expression

viewCall ::
  forall r.
  (Members '[Reader SizeInfo] r) =>
  Expression ->
  Sem r (Maybe (FunCall' Expression))
viewCall = \case
  ExpressionIden (IdenFunction x) ->
    return (Just (singletonCall x))
  ExpressionApplication (Application f x impl)
    | isImplicitOrInstance impl -> viewCall f -- implicit arguments are ignored
    | otherwise -> do
        c <- viewCall f
        x' <- callArg
        return $ over callArgs (`snoc` x') <$> c
    where
      callArg :: Sem r (FunCallArg' Expression)
      callArg = do
        lt <- (^. callRow) <$> lessThan
        eq <- (^. callRow) <$> equalTo
        let cr = CallRow (lt `mplus` eq)
        return
          FunCallArg
            { _argRow = cr,
              _argExpression = x
            }
        where
          lessThan :: Sem r CallRow
          lessThan = case viewExpressionAsPattern x of
            Nothing -> return (CallRow Nothing)
            Just x' -> do
              s <- asks (findIndex (elem x') . (^. sizeSmaller))
              return $ case s of
                Nothing -> CallRow Nothing
                Just s' -> CallRow (Just (s', RLe))
          equalTo :: Sem r CallRow
          equalTo =
            case viewExpressionAsPattern x of
              Just x' -> do
                s <- asks (elemIndex x' . (^. sizeEqual))
                return $ case s of
                  Nothing -> CallRow Nothing
                  Just s' -> CallRow (Just (s', REq))
              Nothing -> return (CallRow Nothing)
  _ -> return Nothing
  where
    singletonCall :: FunctionName -> FunCall' expr
    singletonCall r = FunCall r []

addCall :: forall expr. FunctionName -> FunCall' expr -> CallMap' expr -> CallMap' expr
addCall fun c = over callMap (HashMap.alter (Just . insertCall c) fun)
  where
    insertCall ::
      FunCall' expr ->
      Maybe (HashMap FunctionName [FunCall' expr]) ->
      HashMap FunctionName [FunCall' expr]
    insertCall f = \case
      Nothing -> singl f
      Just m' -> addFunCall f m'

    singl :: FunCall' expr -> HashMap FunctionName [FunCall' expr]
    singl f = HashMap.singleton (f ^. callRef) [f]

    addFunCall ::
      FunCall' expr ->
      HashMap FunctionName [FunCall' expr] ->
      HashMap FunctionName [FunCall' expr]
    addFunCall fc = HashMap.insertWith (flip (<>)) (fc ^. callRef) [fc]

registerFunctionDef ::
  forall expr r.
  (Members '[State (CallMap' expr)] r) =>
  Proxy expr ->
  FunctionDef ->
  Sem r ()
registerFunctionDef Proxy f = modify' @(CallMap' expr) (set (callMapScanned . at (f ^. funDefName)) (Just f))

registerCall ::
  forall expr r.
  (Members '[State (CallMap' expr), Reader FunctionName] r) =>
  FunCall' expr ->
  Sem r ()
registerCall c = do
  fun <- ask
  modify (addCall fun c)
