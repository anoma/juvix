module Commands.Dev.Anoma.Base where

import Anoma.Effect (Anoma)
import Anoma.Effect qualified as Anoma
import Commands.Base hiding (Atom)
import Commands.Dev.Anoma.Prove.Options.ProveArg
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as BC
import Juvix.Compiler.Nockma.Encoding.ByteString
import Juvix.Compiler.Nockma.Pretty
import Juvix.Compiler.Nockma.Translation.FromSource qualified as Nockma
import Juvix.Compiler.Nockma.Translation.FromTree (foldTermsOrNil)

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
  ProveArgList f -> do
    bs <- readAppFile f
    let bss = map Base64.decodeLenient (BC.split '\n' bs)
    atoms <- map TermAtom <$> mapM decodeAtom bss
    return (Anoma.RunNockmaArgTerm (foldTermsOrNil atoms))
  ProveArgFile ArgFileSpec {..} -> do
    bs <- asBytes <$> readAppFile _argFileSpecFile
    toArg bs
    where
      toArg :: ByteString -> Sem r Anoma.RunNockmaArg
      toArg = case _argFileSpecDecoding of
        DecodingAtom -> fmap (Anoma.RunNockmaArgTerm . toNock) . decodeAtom
        DecodingJammed -> return . Anoma.RunNockmaArgJammed
        DecodingByteArray -> \bs -> do
          payload <- decodeAtom bs
          let cell :: Nockma.Term Natural = (fromIntegral @_ @Natural (BS.length bs)) # payload
          return (Anoma.RunNockmaArgTerm cell)

      asBytes :: ByteString -> ByteString
      asBytes = case _argFileSpecEncoding of
        EncodingBytes -> id
        EncodingBase64 -> Base64.decodeLenient
  where
    readAppFile :: AppPath File -> Sem r ByteString
    readAppFile f = fromAppPathFile f >>= readFileBS'

    decodeAtom :: ByteString -> Sem r (Nockma.Atom Natural)
    decodeAtom =
      asSimpleErrorShow @NockNaturalNaturalError
        . byteStringToAtom @Natural

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
