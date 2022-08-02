-- TODO: MOVE
module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Ann where

import Juvix.Compiler.Concrete.Pretty.Base qualified as P

data Eann
  = Highlight
  | ScopedAnn P.Ann
