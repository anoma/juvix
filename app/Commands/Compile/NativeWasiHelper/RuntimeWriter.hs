module Commands.Compile.NativeWasiHelper.RuntimeWriter where

import Commands.Base
import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FE
import Juvix.Extra.Paths

writeRuntime ::
  forall r.
  (Members '[App, EmbedIO] r) =>
  BS.ByteString ->
  Sem r ()
writeRuntime runtime = do
  buildDir <- askBuildDir
  let includeDir :: Path Abs Dir
      includeDir = juvixIncludeDir buildDir

      writeRuntimeFile :: BS.ByteString -> Sem r ()
      writeRuntimeFile =
        liftIO
          . BS.writeFile (toFilePath (buildDir <//> $(mkRelFile "libjuvix.a")))

      writeHeader :: Path Rel File -> BS.ByteString -> Sem r ()
      writeHeader filePath contents = liftIO $ do
        ensureDir (includeDir <//> parent filePath)
        liftIO (BS.writeFile (toFilePath (includeDir <//> filePath)) contents)

  writeRuntimeFile runtime
  mapM_ (uncurry writeHeader) headersDir
  where
    headersDir :: [(Path Rel File, BS.ByteString)]
    headersDir = map (first relFile) $(FE.makeRelativeToProject "runtime/c/include" >>= FE.embedDir)
