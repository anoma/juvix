module Juvix.Compiler.Mono.Extra
  ( module Juvix.Compiler.Mono.Extra,
    module Juvix.Compiler.Mono.Language,
  )
where

import Juvix.Compiler.Mono.Language

foldApplication :: Expression -> [Expression] -> Expression
foldApplication f args = case args of
  [] -> f
  (a : as) -> foldApplication (ExpressionApplication (Application f a)) as
