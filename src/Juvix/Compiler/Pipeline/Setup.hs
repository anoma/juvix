module Juvix.Compiler.Pipeline.Setup where

import Data.FileEmbed qualified as FE
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

stdlibDir :: [(FilePath, Text)]
stdlibDir =
  let stdlibFiles :: [(FilePath, Text)]
      stdlibFiles = second decodeUtf8 <$> $(FE.makeRelativeToProject "juvix-stdlib" >>= FE.embedDir)
      isMjuvixFile :: (FilePath, Text) -> Bool
      isMjuvixFile (fp, _) = takeExtension fp == ".juvix"
   in filter isMjuvixFile stdlibFiles

entrySetup ::
  Members '[Reader EntryPoint, Files] r =>
  Sem r EntryPoint
entrySetup = do
  e <- ask
  let root = e ^. entryPointRoot
  unless (e ^. entryPointNoStdlib) (registerStdlib (first (root </>) <$> stdlibDir))
  return e
