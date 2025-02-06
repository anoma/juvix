{-# LANGUAGE UndecidableInstances #-}

module Commands.Compile.RiscZeroRust.Options
  ( module Commands.Compile.RiscZeroRust.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data RiscZeroRustOptions (k :: InputKind) = RiscZeroRustOptions
  { _riscZeroRustCompileCommonOptions :: CompileCommonOptions k,
    _riscZeroRustOutputDir :: Maybe (AppPath Dir)
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (RiscZeroRustOptions k)

makeLenses ''RiscZeroRustOptions

parseRiscZeroRust :: (SingI k) => Parser (RiscZeroRustOptions k)
parseRiscZeroRust = do
  _riscZeroRustCompileCommonOptions <- parseCompileCommonOptions' False
  _riscZeroRustOutputDir <- optional (parseGenericOutputDir mempty)
  pure RiscZeroRustOptions {..}

instance EntryPointOptions (RiscZeroRustOptions k) where
  applyOptions opts =
    set entryPointPipeline (Just PipelineExec)
      . set entryPointTarget (Just TargetRust)
      . applyOptions (opts ^. riscZeroRustCompileCommonOptions)
