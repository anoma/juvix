module Juvix.Compiler.Backend.Isabelle.Extra where

import Juvix.Compiler.Backend.Isabelle.Language

mkApp :: Expression -> [Expression] -> Expression
mkApp fn = \case
  [] -> fn
  arg : args -> mkApp (ExprApp (Application fn arg)) args
