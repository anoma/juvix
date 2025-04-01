module Juvix.Compiler.Nockma.EvalCompiled where

import Juvix.Compiler.Nockma.Anoma
import Juvix.Compiler.Nockma.Data.Module
import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty (ppTrace)
import Juvix.Prelude

evalCompiledNock :: (Members '[State OpCounts, Reader EvalOptions, Output (Term Natural)] r) => Storage Natural -> Term Natural -> Term Natural -> Sem r (Term Natural)
evalCompiledNock storage stack mainTerm = do
  evalT <-
    runError @(ErrNockNatural Natural)
      . runError @(NockEvalError Natural)
      . runReader storage
      $ evalProfile stack mainTerm
  case evalT of
    Left e -> error (show e)
    Right ev -> case ev of
      Left e -> error (ppTrace e)
      Right res -> return res

evalCompiledNock' :: (Members '[State OpCounts, Reader EvalOptions, Output (Term Natural)] r) => Term Natural -> Term Natural -> Sem r (Term Natural)
evalCompiledNock' = evalCompiledNock emptyStorage

evalCompiledNockModule :: forall r. (Members '[State OpCounts, Reader EvalOptions, Output (Term Natural)] r) => ModuleTable -> Module -> Sem r (Term Natural)
evalCompiledNockModule mtab md = do
  evalCompiledNock (mkModuleStorage mtab) (getModuleCode md) (anomaCall [])

evalCompiledNockModule' :: (Members '[State OpCounts, Reader EvalOptions, Output (Term Natural)] r) => Module -> Sem r (Term Natural)
evalCompiledNockModule' md = do
  evalCompiledNockModule mempty md
