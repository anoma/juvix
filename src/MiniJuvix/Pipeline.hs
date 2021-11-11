module MiniJuvix.Pipeline
  ( -- * Compiler configuration-related data structures
    CompilerMode (..),
    Config (..),
    WriteToFsBehavior (..),
    Pass (..),
    Backend (..),
  )
where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

import MiniJuvix.Error
import MiniJuvix.Utils.Prelude (Eq, FilePath, Maybe, Ord, Show, Text)

--------------------------------------------------------------------------------

data CompilerMode
  = ReplMode
  | BuildMode Config FilePath
  | TestMode Config FilePath

data Config = Config
  { _configPass :: Pass,
    _configBackend :: Backend,
    _configOutputDirectory :: Maybe FilePath,
    _configWriteToFsBehavior :: WriteToFsBehavior
  }

data Pass
  = Parsing
  | Desugaring
  | Typechecking
  | Compiling
  deriving stock (Show)

data Backend = LLVM
  deriving (Eq, Ord, Show)

data WriteToFsBehavior = OverwriteTargetFiles | WriteIfDoesNotExist

runAndLogErrs :: MiniJuvix a -> IO ()
runAndLogErrs m = runMiniJuvix m >>= \(_, errs) -> logErrs errs

-- runTestWith :: FilePath -> Config -> IO ()
-- runTestWith filePath config = case _configPass config of
--   Parsing -> undefined 
--   Desugaring -> undefined
--   Typechecking -> runAndLogErrs $ depAnalPass filePath >>= parsePass >>= checkPass
--   Compiling -> undefined