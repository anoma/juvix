{-# LANGUAGE UndecidableInstances #-}

module Commands.Dev.DevCompile.Casm.Options
  ( module Commands.Dev.DevCompile.Casm.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data CasmOptions (k :: InputKind) = CasmOptions
  { _casmCompileCommonOptions :: CompileCommonOptions k
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (CasmOptions k)

makeLenses ''CasmOptions

parseCasm :: (SingI k) => Parser (CasmOptions k)
parseCasm = do
  _casmCompileCommonOptions <- parseCompileCommonOptions
  pure CasmOptions {..}

instance EntryPointOptions (CasmOptions k) where
  applyOptions = applyOptions . (^. casmCompileCommonOptions)
