module Commands.Dev.Anoma.Base where

import Anoma.Effect
import Commands.Base hiding (Atom)
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma

-- | Calls Anoma.Protobuf.NockService.Prove
runNock :: forall r. (Members '[Error SimpleError, Anoma] r, Members AppEffects r) => AppPath File -> Maybe (AppPath File) -> Sem r RunNockmaResult
runNock programFile margsFile = do
  afile <- fromAppPathFile programFile
  argsFile <- mapM fromAppPathFile margsFile
  parsedArgs <- runAppError @JuvixError (mapM Nockma.cueJammedFileOrPretty argsFile)
  parsedTerm <- runAppError @JuvixError (Nockma.cueJammedFileOrPretty afile)
  case parsedTerm of
    TermAtom {} -> exitFailMsg "Expected nockma input to be a cell"
    t@(TermCell {}) -> go t (maybe [] unfoldList parsedArgs)
  where
    go :: Term Natural -> [Term Natural] -> Sem r RunNockmaResult
    go t args =
      runNockma
        RunNockmaInput
          { _runNockmaProgram = t,
            _runNockmaArgs = args
          }
