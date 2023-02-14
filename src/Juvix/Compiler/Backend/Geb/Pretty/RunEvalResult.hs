module Juvix.Compiler.Backend.Geb.Pretty.RunEvalResult where

import Juvix.Compiler.Backend.Geb.Evaluator.Data.RunEvalResult
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty.Base qualified as Geb
import Juvix.Compiler.Backend.Geb.Pretty.Options
import Juvix.Compiler.Backend.Geb.Pretty.Values qualified as GebValue
import Juvix.Data.CodeAnn

class PrettyCode c where
  ppCode :: Member (Reader Options) r => c -> Sem r (Doc Ann)

instance PrettyCode RunEvalResult where
  ppCode = \case
    RunEvalResultMorphism m -> Geb.ppCode m
    RunEvalResultGebValue v -> GebValue.ppCode v
