module MiniJuvix.Pipeline
  ( -- * Compiler configuration-related data structures
    Config (..),
    WriteToFsBehavior (..),
    Pass (..),
    Backend (..),
    Mode (..),
  )
where

--------------------------------------------------------------------------------

import MiniJuvix.Error
import MiniJuvix.Utils.Prelude (Eq, FilePath, Maybe, Ord, Show, Text)

--------------------------------------------------------------------------------

data Mode
  = ReplMode
  | CheckMode Config FilePath
  | CompileMode Config FilePath
  | TestMode Config FilePath

data Config
  = Config
      { _configPass :: Pass,
        _configBackend :: Backend,
        _configOutputDirectory :: Maybe FilePath,
        _configWriteToFsBehavior :: WriteToFsBehavior
      }

data Pass
  = Parsing
  | Desugaring
  | Checking
  | Compiling
  deriving stock (Show)

data Backend = LLVM
  deriving stock (Eq, Ord, Show)

data WriteToFsBehavior = OverwriteTargetFiles | WriteIfDoesNotExist
-- run' :: MiniJuvix a -> IO ()
-- run' m = runMiniJuvix m >>= \(_, errs) -> logErrors errs

-- runTestWith :: FilePath -> Config -> IO ()
-- runTestWith filePath config = case _configPass config of
--   Parsing -> undefined
--   Desugaring -> undefined
-- Checking -> run' $ filePath >>= parsingPass >>= checkingPass
--   Compiling -> undefined

-- runMiniJuvix :: MiniJuvix a -> IO ()
-- runMiniJuvix = undefined
