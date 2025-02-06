{-# LANGUAGE UndecidableInstances #-}

module Commands.Compile.Anoma.Options
  ( module Commands.Compile.Anoma.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data AnomaOptions (k :: InputKind) = AnomaOptions
  { _anomaCompileCommonOptions :: CompileCommonOptions k
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (AnomaOptions k)

makeLenses ''AnomaOptions

parseAnoma :: (SingI k) => Parser (AnomaOptions k)
parseAnoma = do
  _anomaCompileCommonOptions <- parseCompileCommonOptions
  pure AnomaOptions {..}

instance EntryPointOptions (AnomaOptions k) where
  applyOptions opts =
    set entryPointPipeline (Just PipelineExec)
      . set entryPointTarget (Just TargetAnoma)
      . applyOptions (opts ^. anomaCompileCommonOptions)
