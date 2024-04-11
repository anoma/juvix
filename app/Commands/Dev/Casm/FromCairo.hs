module Commands.Dev.Casm.FromCairo where

import Commands.Base
import Commands.Dev.Casm.FromCairo.Options
import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Juvix.Compiler.Backend.Cairo.Extra.Deserialization qualified as Cairo
import Juvix.Compiler.Casm.Data.Result qualified as Casm
import Juvix.Compiler.Casm.Pretty qualified as Casm
import Juvix.Compiler.Casm.Translation.FromCairo qualified as Casm

runCommand :: forall r. (Members '[EmbedIO, App] r) => CasmFromCairoOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  bs <- liftIO $ BS.readFile (toFilePath afile)
  case decode bs of
    Just r -> do
      let Casm.Result {..} = Casm.fromCairo (Cairo.deserialize r)
      renderStdOut (Casm.ppProgram _resultCode)
    Nothing ->
      exitFailMsg "error reading input file"
  where
    file :: AppPath File
    file = opts ^. casmFromCairoInputFile
