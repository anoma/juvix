module Juvix.Compiler.Backend.Lean.Pretty.Keywords where

import Juvix.Data.CodeAnn

-- Define Lean-specific keywords here.
kwDefinition :: Doc Ann
kwDefinition = keyword "def"

kwTheorem :: Doc Ann
kwTheorem = keyword "theorem"

-- [...]