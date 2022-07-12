module Commands.Internal.Scope where

import GlobalOptions
import Juvix.Prelude hiding (Doc)
import Juvix.Syntax.Concrete.Scoped.Pretty qualified as Scoper
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

mkScopePrettyOptions :: GlobalOptions -> ScopeOptions -> Scoper.Options
mkScopePrettyOptions g ScopeOptions {..} =
  Scoper.defaultOptions
    { Scoper._optShowNameIds = g ^. globalShowNameIds,
      Scoper._optInlineImports = _scopeInlineImports
    }
