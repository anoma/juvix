module Commands.Extra where

import qualified MiniJuvix.Translation.ScopedToAbstract as A
import qualified Data.Text as Text
import Control.Monad.Extra
import Options.Applicative
import Options.Applicative.Help.Pretty
import qualified MiniJuvix.Syntax.Abstract.Pretty.Base as A
import MiniJuvix.Prelude hiding (Doc)

parseInputFile :: Parser FilePath
parseInputFile =
 argument
      str
      ( metavar "MINIJUVIX_FILE"
          <> help "Path to a .mjuvix file"
      )
