module Commands.Dev.Anoma.Base where

import Anoma.Effect (Anoma)
import Anoma.Effect qualified as Anoma
import Commands.Base hiding (Atom)
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma

cellOrFail ::
  forall x r a.
  (Member App r, Subset x r) =>
  Term Natural ->
  (Term Natural -> Sem x a) ->
  Sem r a
cellOrFail term f = case term of
  TermAtom {} -> exitFailMsg "Expected nockma input to be a cell"
  t@(TermCell {}) -> inject (f t)

-- | Calls Anoma.Protobuf.NockService.Prove
runNock ::
  forall r.
  (Members '[Error SimpleError, Anoma] r, Members AppEffects r) =>
  AppPath File ->
  Maybe (AppPath File) ->
  Sem r Anoma.RunNockmaResult
runNock programFile margsFile = do
  afile <- fromAppPathFile programFile
  argsFile <- mapM fromAppPathFile margsFile
  parsedArgs <- runAppError @JuvixError (mapM Nockma.cueJammedFileOrPretty argsFile)
  parsedTerm <- runAppError @JuvixError (Nockma.cueJammedFileOrPretty afile)
  cellOrFail parsedTerm (go (maybe [] unfoldList parsedArgs))
  where
    go :: [Term Natural] -> Term Natural -> Sem r Anoma.RunNockmaResult
    go args t =
      Anoma.runNockma
        Anoma.RunNockmaInput
          { _runNockmaProgram = t,
            _runNockmaArgs = args
          }

-- | Calls Anoma.Protobuf.Mempool.AddTransaction
addTransaction ::
  forall r.
  (Members '[Error SimpleError, Anoma] r, Members AppEffects r) =>
  AppPath File ->
  Sem r ()
addTransaction programFile = do
  afile <- fromAppPathFile programFile
  parsedTerm <- runAppError @JuvixError (Nockma.cueJammedFileOrPretty afile)
  cellOrFail parsedTerm $ \t ->
    Anoma.addTransaction
      Anoma.AddTransactionInput
        { _addTransactionInputCandidate = t
        }
