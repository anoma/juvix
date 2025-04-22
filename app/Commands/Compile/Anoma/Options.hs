{-# LANGUAGE UndecidableInstances #-}

module Commands.Compile.Anoma.Options
  ( module Commands.Compile.Anoma.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data AnomaOptions (k :: InputKind) = AnomaOptions
  { _anomaCompileCommonOptions :: CompileCommonOptions k,
    _anomaModular :: Bool,
    _anomaOutputText :: Bool,
    _anomaNoStdlib :: Bool
  }

deriving stock instance (Typeable k, Data (InputFileType k)) => Data (AnomaOptions k)

makeLenses ''AnomaOptions

parseAnoma :: (SingI k) => Parser (AnomaOptions k)
parseAnoma = do
  _anomaCompileCommonOptions <- parseCompileCommonOptions
  _anomaModular <-
    switch
      ( long "modular"
          <> help "Generate modular code"
      )
  _anomaOutputText <-
    switch
      ( long "text"
          <> help "Output code in textual format"
      )
  _anomaNoStdlib <-
    switch
      ( long "no-anomalib"
          <> help "Do not bundle the Anoma standard library"
      )
  pure AnomaOptions {..}

instance EntryPointOptions (AnomaOptions k) where
  applyOptions opts =
    set entryPointPipeline (Just PipelineExec)
      . set entryPointTarget (Just TargetAnoma)
      . set entryPointNoAnomaStdlib (opts ^. anomaNoStdlib)
      . applyOptions (opts ^. anomaCompileCommonOptions)
