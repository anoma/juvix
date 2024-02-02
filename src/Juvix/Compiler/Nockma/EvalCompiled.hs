module Juvix.Compiler.Nockma.EvalCompiled where

import Juvix.Compiler.Nockma.Evaluator
import Juvix.Compiler.Nockma.Language
import Juvix.Compiler.Nockma.Pretty (ppTrace)
import Juvix.Compiler.Nockma.Translation.FromAsm
import Juvix.Prelude

compileAndRunNock' :: (Members '[Reader EvalOptions, Output (Term Natural)] r) => CompilerOptions -> ConstructorInfos -> [CompilerFunction] -> CompilerFunction -> Sem r (Term Natural)
compileAndRunNock' opts constrs funs mainfun =
  let Cell nockSubject t = runCompilerWith opts constrs funs mainfun
   in evalCompiledNock' nockSubject t

evalCompiledNock' :: (Members '[Reader EvalOptions, Output (Term Natural)] r) => Term Natural -> Term Natural -> Sem r (Term Natural)
evalCompiledNock' stack mainTerm = do
  evalT <-
    runError @(ErrNockNatural Natural)
      . runError @(NockEvalError Natural)
      $ eval stack mainTerm
  case evalT of
    Left e -> error (show e)
    Right ev -> case ev of
      Left e -> error (ppTrace e)
      Right res -> return res

-- | Used in testing and app
getStack :: StackId -> Term Natural -> Term Natural
getStack st m =
  fromRight'
    . run
    . runError @(NockEvalError Natural)
    . topEvalCtx
    . subTerm m
    $ stackPath st
