module Commands.Extra where

import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

parseInputFile :: Parser FilePath
parseInputFile =
  argument
    str
    ( metavar "MINIJUVIX_FILE"
        <> help "Path to a .mjuvix file"
    )
