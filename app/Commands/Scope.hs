module Commands.Scope where

import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

newtype ScopeOptions = ScopeOptions
  { _scopeInlineImports :: Bool
  }

makeLenses ''ScopeOptions

parseScope :: Parser ScopeOptions
parseScope = do
  _scopeInlineImports <-
    switch
      ( long "inline-imports"
          <> help "Show the code of imported modules next to the import statement"
      )
  pure ScopeOptions {..}
