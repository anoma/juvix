module Commands.Dev.Core.Read.Options where

import CommonOptions
import Juvix.Compiler.Core.Data.TransformationId.Parser
import Juvix.Compiler.Core.Pretty.Options qualified as Core

data CoreReadOptions = CoreReadOptions
  { _coreReadTransformations :: [TransformationId],
    _coreReadShowDeBruijn :: Bool,
    _coreReadInputFile :: Path
  }
  deriving stock (Data)

makeLenses ''CoreReadOptions

instance CanonicalProjection CoreReadOptions Core.Options where
  project c =
    Core.defaultOptions
      { Core._optShowDeBruijnIndices = c ^. coreReadShowDeBruijn
      }

parseCoreReadOptions :: Parser CoreReadOptions
parseCoreReadOptions = do
  _coreReadShowDeBruijn <- optDeBruijn
  _coreReadTransformations <-
    option
      (eitherReader parseTransf)
      ( long "transforms"
          <> short 't'
          <> value mempty
          <> metavar "[Transform]"
          <> help "comma sep list of transformations. Available: lifting"
      )
  _coreReadInputFile <- parseInputJuvixCoreFile
  pure CoreReadOptions {..}
  where
    parseTransf :: String -> Either String [TransformationId]
    parseTransf = mapLeft unpack . parseTransformations . pack
