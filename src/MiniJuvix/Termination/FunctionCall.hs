module MiniJuvix.Termination.FunctionCall
  ( module MiniJuvix.Termination.FunctionCall,
  )
where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language.Extra
import MiniJuvix.Termination.Types

viewCall ::
  forall r.
  Members '[Reader SizeInfo] r =>
  Expression ->
  Sem r (Maybe FunCall)
viewCall = \case
  ExpressionApplication (Application f _ Implicit) -> viewCall f
  ExpressionApplication (Application f x _) -> do
    c <- viewCall f
    x' <- callArg
    return $ over callArgs (`snoc` x') <$> c
    where
      callArg :: Sem r (CallRow, Expression)
      callArg = do
        lt <- lessThan
        eq <- equalTo
        return (CallRow ((lt ^. callRow) `mplus` (eq ^. callRow)), x)
        where
          lessThan :: Sem r CallRow
          lessThan = case x of
            ExpressionIden (IdenVar v) -> do
              s :: Maybe Int <- asks (HashMap.lookup v . (^. sizeSmaller))
              return $ case s of
                Nothing -> CallRow Nothing
                Just s' -> CallRow (Just (s', RLe))
            _ -> return (CallRow Nothing)
          equalTo :: Sem r CallRow
          equalTo = do
            case viewExpressionAsPattern x of
              Just x' -> do
                s <- asks (elemIndex x' . (^. sizeEqual))
                return $ case s of
                  Nothing -> CallRow Nothing
                  Just s' -> CallRow (Just (s', REq))
              _ -> return (CallRow Nothing)
  ExpressionIden (IdenFunction x) ->
    return (Just (singletonCall x))
  _ -> return Nothing
  where
    singletonCall :: FunctionRef -> FunCall
    singletonCall r = FunCall r []

addCall :: FunctionRef -> FunCall -> CallMap -> CallMap
addCall fun c = over callMap (HashMap.alter (Just . insertCall c) fun)
  where
    insertCall ::
      FunCall ->
      Maybe (HashMap FunctionRef [FunCall]) ->
      HashMap FunctionRef [FunCall]
    insertCall f = \case
      Nothing -> singl f
      Just m' -> addFunCall f m'

    singl :: FunCall -> HashMap FunctionRef [FunCall]
    singl f = HashMap.singleton (f ^. callRef) [f]

    addFunCall ::
      FunCall ->
      HashMap FunctionRef [FunCall] ->
      HashMap FunctionRef [FunCall]
    addFunCall fc = HashMap.insertWith (flip (<>)) (fc ^. callRef) [fc]

registerCall ::
  Members '[State CallMap, Reader FunctionRef, Reader SizeInfo] r =>
  FunCall ->
  Sem r ()
registerCall c = do
  fun <- ask
  modify (addCall fun c)
