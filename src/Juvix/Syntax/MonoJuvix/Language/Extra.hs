module Juvix.Syntax.MonoJuvix.Language.Extra
  ( module Juvix.Syntax.MonoJuvix.Language.Extra,
    module Juvix.Syntax.MonoJuvix.Language,
  )
where

import Juvix.Syntax.MonoJuvix.Language

foldApplication :: Expression -> [Expression] -> Expression
foldApplication f args = case args of
  [] -> f
  (a : as) -> foldApplication (ExpressionApplication (Application f a)) as
