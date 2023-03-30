module Commands.Dev.Core.Repl.Options where

import CommonOptions
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data CoreReplOptions = CoreReplOptions
  { _coreReplShowDeBruijn :: Bool,
    _coreReplShowIdentIds :: Bool,
    _coreReplShowArgsNum :: Bool
  }
  deriving stock (Data)

makeLenses ''CoreReplOptions

instance CanonicalProjection CoreReplOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreReplShowDeBruijn,
        Core._optShowIdentIds = c ^. coreReplShowIdentIds,
        Core._optShowArgsNum = c ^. coreReplShowArgsNum
      }

parseCoreReplOptions :: Parser CoreReplOptions
parseCoreReplOptions = do
  _coreReplShowDeBruijn <- optDeBruijn
  _coreReplShowIdentIds <- optIdentIds
  _coreReplShowArgsNum <- optArgsNum
  pure CoreReplOptions {..}
