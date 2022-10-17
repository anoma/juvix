module Commands.Dev.Scope.Options where

import CommonOptions
import GlobalOptions
import Juvix.Compiler.Concrete.Pretty qualified as Scoper

data ScopeOptions = ScopeOptions
  { _scopeInlineImports :: Bool,
    _scopeInputFile :: Path
  }
  deriving stock (Data)

makeLenses ''ScopeOptions

parseScope :: Parser ScopeOptions
parseScope = do
  _scopeInlineImports <-
    switch
      ( long "inline-imports"
          <> help "Show the code of imported modules next to the import statement"
      )
  _scopeInputFile <- parseInputJuvixFile
  pure ScopeOptions {..}

instance CanonicalProjection (GlobalOptions, ScopeOptions) Scoper.Options where
  project (g, ScopeOptions {..}) =
    Scoper.defaultOptions
      { Scoper._optShowNameIds = g ^. globalShowNameIds,
        Scoper._optInlineImports = _scopeInlineImports,
        Scoper._optNoApe = g ^. globalNoApe
      }
