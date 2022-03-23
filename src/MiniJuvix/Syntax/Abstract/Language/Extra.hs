module MiniJuvix.Syntax.Abstract.Language.Extra
  ( module MiniJuvix.Syntax.Abstract.Language,
    module MiniJuvix.Syntax.Abstract.Language.Extra,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language

smallerPatternVariables :: Pattern -> [VarName]
smallerPatternVariables p = case p of
  PatternVariable {} -> []
  PatternWildcard {} -> []
  PatternEmpty {} -> []
  PatternConstructorApp app -> appVariables app
  where
    appVariables :: ConstructorApp -> [VarName]
    appVariables (ConstructorApp _ ps) = concatMap patternVariables ps

    patternVariables :: Pattern -> [VarName]
    patternVariables pat = case pat of
      PatternVariable v -> [v]
      PatternWildcard {} -> []
      PatternEmpty {} -> []
      PatternConstructorApp app -> appVariables app

viewApp :: Expression -> (Expression, [Expression])
viewApp e = case e of
  ExpressionIden {} -> (e, [])
  ExpressionApplication (Application l r) ->
    second (`snoc` r) (viewApp l)
  ExpressionUniverse {} -> (e, [])
  ExpressionFunction {} -> (e, [])
  ExpressionLiteral {} -> (e, [])

viewExpressionAsPattern :: Expression -> Maybe Pattern
viewExpressionAsPattern e = case viewApp e of
  (f, args)
    | Just c <- getConstructor f -> do
      args' <- mapM viewExpressionAsPattern args
      Just $ PatternConstructorApp (ConstructorApp c args')
  (f, [])
    | Just v <- getVariable f -> Just (PatternVariable v)
  _ -> Nothing
  where
    getConstructor :: Expression -> Maybe Name
    getConstructor f = case f of
      ExpressionIden (IdenConstructor n) -> Just n
      _ -> Nothing
    getVariable :: Expression -> Maybe VarName
    getVariable f = case f of
      ExpressionIden (IdenVar n) -> Just n
      _ -> Nothing
