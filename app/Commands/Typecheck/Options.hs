module Commands.Typecheck.Options where

import Commands.Dev.Internal.Typecheck.Options qualified as Internal
import CommonOptions
import Data.List.NonEmpty qualified as NonEmpty

newtype TypecheckOptions = TypecheckOptions
  { _typecheckInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''TypecheckOptions

parseTypecheck :: Parser TypecheckOptions
parseTypecheck = do
  _typecheckInputFile <- parseInputFiles (NonEmpty.fromList [FileExtJuvix, FileExtJuvixMarkdown])
  pure TypecheckOptions {..}

instance CanonicalProjection TypecheckOptions Internal.InternalTypeOptions where
  project o =
    Internal.InternalTypeOptions
      { _internalTypePrint = False,
        _internalTypeInputFile = o ^. typecheckInputFile
      }
