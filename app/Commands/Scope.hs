module Commands.Scope where

import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

data ScopeOptions = ScopeOptions
  { _scopeInputFiles :: NonEmpty FilePath,
    _scopeInlineImports :: Bool
  }

makeLenses ''ScopeOptions

parseScope :: Parser ScopeOptions
parseScope = do
  _scopeInputFiles <-
    some1 $
      argument
        str
        ( metavar "MINIJUVIX_FILE(s)"
            <> help "Path to one ore more MiniJuvix files"
            <> action "file"
        )
  _scopeInlineImports <-
    switch
      ( long "inline-imports"
          <> help "Show the code of imported modules next to the import statement"
      )
  _scopeNoColors <-
    switch
      ( long "no-colors"
          <> help "Disable ANSI formatting"
      )
  pure ScopeOptions {..}
