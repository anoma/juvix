module Commands.Dev.Scope where

import GlobalOptions
import Juvix.Compiler.Concrete.Pretty qualified as Scoper
import Juvix.Prelude hiding (Doc)
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

instance CanonicalProjection (GlobalOptions, ScopeOptions) Scoper.Options where
  project (g, ScopeOptions {..}) =
    Scoper.defaultOptions
      { Scoper._optShowNameIds = g ^. globalShowNameIds,
        Scoper._optInlineImports = _scopeInlineImports
      }
