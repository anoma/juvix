{-# LANGUAGE UndecidableInstances #-}

module Commands.Dev.DevCompile.Rust.Options
  ( module Commands.Dev.DevCompile.Rust.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data RustOptions (k :: InputKind) = RustOptions
  { _rustCompileCommonOptions :: CompileCommonOptions k
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (RustOptions k)

makeLenses ''RustOptions

parseRust :: (SingI k) => Parser (RustOptions k)
parseRust = do
  _rustCompileCommonOptions <- parseCompileCommonOptions
  pure RustOptions {..}

instance EntryPointOptions (RustOptions k) where
  applyOptions opts =
    set entryPointPipeline (Just PipelineExec)
      . set entryPointTarget (Just TargetRust)
      . applyOptions (opts ^. rustCompileCommonOptions)
