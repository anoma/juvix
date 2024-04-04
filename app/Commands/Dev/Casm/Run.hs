module Commands.Dev.Casm.Run where

import Commands.Base
import Commands.Dev.Casm.Run.Options
import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Juvix.Compiler.Casm.Data.InputInfo qualified as Casm
import Juvix.Compiler.Casm.Interpreter qualified as Casm
import Juvix.Compiler.Casm.Translation.FromSource qualified as Casm
import Juvix.Compiler.Casm.Validate qualified as Casm

runCommand :: forall r. (Members '[EmbedIO, App] r) => CasmRunOptions -> Sem r ()
runCommand opts = do
  afile :: Path Abs File <- fromAppPathFile file
  dfile :: Maybe (Path Abs File) <- maybe (return Nothing) (fromAppPathFile >=> return . Just) (opts ^. casmRunDataFile)
  inputInfo <-
    case dfile of
      Just df -> do
        bs <- liftIO $ BS.readFile (toFilePath df)
        return $ fromJust $ decode bs
      Nothing -> return $ Casm.InputInfo mempty
  s <- readFile afile
  case Casm.runParser afile s of
    Left err -> exitJuvixError (JuvixError err)
    Right (labi, code) ->
      case Casm.validate labi code of
        Left err -> exitJuvixError (JuvixError err)
        Right () -> print (Casm.runCode inputInfo labi code)
  where
    file :: AppPath File
    file = opts ^. casmRunInputFile
