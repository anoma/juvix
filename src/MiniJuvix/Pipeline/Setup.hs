module MiniJuvix.Pipeline.Setup where

import Data.FileEmbed qualified as FE
import MiniJuvix.Pipeline.EntryPoint
import MiniJuvix.Prelude

stdlibDir :: [(FilePath, Text)]
stdlibDir =
  let stdlibFiles :: [(FilePath, Text)]
      stdlibFiles = second decodeUtf8 <$> $(FE.makeRelativeToProject "minijuvix-stdlib" >>= FE.embedDir)
      isMjuvixFile :: (FilePath, Text) -> Bool
      isMjuvixFile (fp, _) = takeExtension fp == ".mjuvix"
   in filter isMjuvixFile stdlibFiles

entrySetup ::
  Member Files r =>
  EntryPoint ->
  Sem r EntryPoint
entrySetup e = do
  let root = e ^. entryPointRoot
  unless (e ^. entryPointNoStdlib) (registerStdlib (first (root </>) <$> stdlibDir))
  return e
