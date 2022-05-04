module MiniJuvix.Syntax.MonoJuvix.Language.Extra
  ( module MiniJuvix.Syntax.MonoJuvix.Language.Extra,
    module MiniJuvix.Syntax.MonoJuvix.Language,
  )
where

import MiniJuvix.Syntax.MonoJuvix.Language

foldApplication :: Expression -> [Expression] -> Expression
foldApplication f args = case args of
  [] -> f
  (a : as) -> foldApplication (ExpressionApplication (Application f a)) as
