module Juvix.Compiler.Nockma.EvalCompiled where

import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty (ppTrace)
import Juvix.Prelude

evalCompiledNock' :: (Members '[Reader EvalOptions, Output (Term Natural)] r) => Term Natural -> Term Natural -> Sem r (Term Natural)
evalCompiledNock' stack mainTerm = do
  evalT <-
    runError @(ErrNockNatural Natural)
      . runError @(NockEvalError Natural)
      $ evalProfile stack mainTerm
  case evalT of
    Left e -> error (show e)
    Right ev -> case ev of
      Left e -> error (ppTrace e)
      Right res -> return res
