module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.FunctionCall
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.FunctionCall,
  )
where

import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data
import Juvix.Prelude

viewCallNew :: Expression -> Maybe (FunctionName, [Expression])
viewCallNew = fmap swap . run . runFail . runOutputList . go
  where
    go :: (Members '[Fail, Output Expression] r) => Expression -> Sem r FunctionName
    go = \case
      ExpressionIden (IdenFunction fun) -> return fun
      ExpressionApplication (Application f arg impl)
        | isImplicitOrInstance impl -> go f -- implicit arguments are ignored
        | otherwise -> do
            fun <- go f
            output arg
            return fun
      _ -> fail

viewCall ::
  forall r.
  (Members '[Reader SizeInfo] r) =>
  Expression ->
  Sem r (Maybe (FunCall' Expression))
viewCall e = do
  si :: SizeInfo <- ask
  let rel :: Pattern -> Expression -> Maybe SizeRel'
      rel pat expr = do
        pexpr <- viewExpressionAsPattern expr
        guard (pexpr `elem` pat ^.. patternSubCosmos) $> RLe
          <|> guard (pexpr == pat) $> REq
  return $ do
    (fun, args) <- viewCallNew e
    return (mkFunCall rel fun (si ^. sizeEqual) args)

registerFunctionDef ::
  forall expr r.
  (Members '[State (HashMap FunctionName FunctionDef)] r) =>
  Proxy expr ->
  FunctionDef ->
  Sem r ()
registerFunctionDef Proxy f = modify' @(HashMap FunctionName FunctionDef) (set (at (f ^. funDefName)) (Just f))

registerCall ::
  forall expr r.
  (Members '[CallMapBuilder' expr, Reader FunctionName] r) =>
  FunCall' expr ->
  Sem r ()
registerCall c = do
  fun <- ask
  addCall fun c
