module Commands.Extra where

import Options.Applicative
import MiniJuvix.Prelude hiding (Doc)

parseInputFile :: Parser FilePath
parseInputFile =
 argument
      str
      ( metavar "MINIJUVIX_FILE"
          <> help "Path to a .mjuvix file"
      )
