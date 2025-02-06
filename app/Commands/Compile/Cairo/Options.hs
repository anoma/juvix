{-# LANGUAGE UndecidableInstances #-}

module Commands.Compile.Cairo.Options
  ( module Commands.Compile.Cairo.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data CairoOptions (k :: InputKind) = CairoOptions
  { _cairoCompileCommonOptions :: CompileCommonOptions k
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (CairoOptions k)

makeLenses ''CairoOptions

parseCairo :: (SingI k) => Parser (CairoOptions k)
parseCairo = do
  _cairoCompileCommonOptions <- parseCompileCommonOptions
  pure CairoOptions {..}

instance EntryPointOptions (CairoOptions k) where
  applyOptions opts =
    set entryPointPipeline (Just PipelineExec)
      . set entryPointTarget (Just TargetCairo)
      . applyOptions (opts ^. cairoCompileCommonOptions)
