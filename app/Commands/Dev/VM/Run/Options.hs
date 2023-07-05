module Commands.Dev.VM.Run.Options where

import CommonOptions
import Juvix.Compiler.VM.Options qualified as VM

data VMRunOptions = VMRunOptions
  { _vmRunInputFile :: AppPath File,
    _vmRunStackSize :: Int,
    _vmRunHeapSize :: Int
  }
  deriving stock (Data)

makeLenses ''VMRunOptions

parseVMRunOptions :: Parser VMRunOptions
parseVMRunOptions = do
  _vmRunInputFile <- parseInputJuvixVMFile
  _vmRunStackSize <- optStackSize
  _vmRunHeapSize <- optHeapSize
  pure VMRunOptions {..}

instance CanonicalProjection VMRunOptions VM.Options where
  project opts =
    VM.defaultOptions
      { VM._optStackSize = opts ^. vmRunStackSize,
        VM._optHeapSize = opts ^. vmRunHeapSize
      }
