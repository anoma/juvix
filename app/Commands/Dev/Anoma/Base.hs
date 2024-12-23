module Commands.Dev.Anoma.Base where

import Anoma.Effect (Anoma)
import Anoma.Effect qualified as Anoma
import Commands.Base hiding (Atom)
import Commands.Dev.Anoma.Prove.Options.ProveArg
import Data.ByteString.Base64 qualified as Base64
import Juvix.Compiler.Nockma.Encoding.ByteString
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

data ParsedArgsMode
  = -- | The args file is a pretty nockma term
    ParsedArgsModePretty
  | -- | The args file is either a pretty nockma term or a jammed nockma term
    ParsedArgsModeJammedOrPretty

-- | Calls Anoma.Protobuf.NockService.Prove
runNock ::
  forall r.
  (Members '[Error SimpleError, Anoma] r, Members AppEffects r) =>
  AppPath File ->
  [ProveArg] ->
  Sem r Anoma.RunNockmaResult
runNock programFile pargs = do
  argsfile <- fromAppPathFile programFile
  parsedTerm <- runAppError @JuvixError (Nockma.cueJammedFileOrPretty argsfile)
  args <- mapM proveArgToTerm pargs
  Anoma.runNockma
    Anoma.RunNockmaInput
      { _runNockmaProgram = parsedTerm,
        _runNockmaArgs = args
      }

proveArgToTerm :: forall r. (Members '[Error SimpleError, Files, App] r) => ProveArg -> Sem r (Term Natural)
proveArgToTerm = \case
  ProveArgNat n -> return (toNock n)
  ProveArgBytes n -> fromAppPathFile n >>= readFileBS' >>= fromBytes
  ProveArgBase64 n -> do
    bs <- Base64.decodeLenient <$> (fromAppPathFile n >>= readFileBS')
    fromBytes bs
  where
    fromBytes :: ByteString -> Sem r (Term Natural)
    fromBytes b = TermAtom <$> asSimpleErrorShow @NockNaturalNaturalError (byteStringToAtom @Natural b)

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
