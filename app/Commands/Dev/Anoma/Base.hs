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
runNock programAppPath pargs = do
  programFile <- fromAppPathFile programAppPath
  parsedProgram <- runAppError @JuvixError (Nockma.cueJammedFileOrPretty programFile)
  args <- mapM prepareArg pargs
  Anoma.runNockma
    Anoma.RunNockmaInput
      { _runNockmaProgram = parsedProgram,
        _runNockmaArgs = args
      }

prepareArg :: forall r. (Members '[Error SimpleError, Files, App] r) => ProveArg -> Sem r Anoma.RunNockmaArg
prepareArg = \case
  ProveArgNat n -> return (Anoma.RunNockmaArgTerm (toNock n))
  ProveArgFile ArgFileSpec {..} ->
    readAppFile _argFileSpecFile >>= fmap toArg . fromBytes
    where
      toArg :: Atom Natural -> Anoma.RunNockmaArg
      toArg
        | _argFileSpecEncoding ^. encodingJammed = Anoma.RunNockmaArgJammed
        | otherwise = Anoma.RunNockmaArgTerm . toNock

      fromBytes :: ByteString -> Sem r (Atom Natural)
      fromBytes b =
        asSimpleErrorShow @NockNaturalNaturalError
          . byteStringToAtom @Natural
          . decode
          $ b
        where
          decode = case _argFileSpecEncoding ^. encodingLayout of
            EncodingBytes -> id
            EncodingBase64 -> Base64.decodeLenient

      readAppFile :: AppPath File -> Sem r ByteString
      readAppFile f = fromAppPathFile f >>= readFileBS'

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
