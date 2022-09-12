module Commands.Dev.Core.Repl.Options where

import CommonOptions
import Juvix.Compiler.Core.Pretty.Options qualified as Core

newtype CoreReplOptions = CoreReplOptions
  { _coreReplShowDeBruijn :: Bool
  }
  deriving stock (Data)

makeLenses ''CoreReplOptions

instance CanonicalProjection CoreReplOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreReplShowDeBruijn
      }

parseCoreReplOptions :: Parser CoreReplOptions
parseCoreReplOptions = do
  _coreReplShowDeBruijn <- optDeBruijn
  pure CoreReplOptions {..}
