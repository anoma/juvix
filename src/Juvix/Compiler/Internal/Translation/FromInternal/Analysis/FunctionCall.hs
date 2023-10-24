module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.FunctionCall
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.FunctionCall,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data
import Juvix.Prelude

viewCall ::
  forall r.
  (Members '[Reader SizeInfo] r) =>
  Expression ->
  Sem r (Maybe FunCall)
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
      callArg :: Sem r (CallRow, Expression)
      callArg = do
        lt <- (^. callRow) <$> lessThan
        eq <- (^. callRow) <$> equalTo
        return (CallRow (lt `mplus` eq), x)
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

registerFunctionDef ::
  (Members '[State CallMap] r) =>
  FunctionDef ->
  Sem r ()
registerFunctionDef f = modify' (set (callMapScanned . at (f ^. funDefName)) (Just f))

registerCall ::
  (Members '[State CallMap, Reader FunctionRef] r) =>
  FunCall ->
  Sem r ()
registerCall c = do
  fun <- ask
  modify (addCall fun c)
