module Juvix.Asm.Translation.FromCore where

import Juvix.Core.Language qualified as Core
import Juvix.Asm.Language
import Data.DList qualified as DL

-- DList for O(1) concatenation
type Code' = DL.DList Instruction

-- Generate code for a single function. Assumes lambda-lifting, i.e., lambdas
-- occur only at the top.
genCode :: Core.Node -> Code
genCode = DL.toList . goToplevel
  where
    unimplemented :: a
    unimplemented = error "not yet implemented"

    goToplevel :: Core.Node -> Code'
    goToplevel node = unimplemented

    go :: [Value] -> Core.Node -> Code'
    go refs = \case
      Core.Var {..} -> unimplemented
      Core.Ident {..} -> unimplemented
      Core.Constant {..} -> unimplemented
      Core.Axiom {..} -> unimplemented
      Core.App {..} -> unimplemented
      Core.BuiltinApp {..} -> unimplemented
      Core.ConstrApp {..} -> unimplemented
      Core.Lambda {..} -> unimplemented
      Core.Let {..} -> unimplemented
      Core.Case {..} -> unimplemented
      Core.If {..} -> unimplemented
      _ -> impossible
