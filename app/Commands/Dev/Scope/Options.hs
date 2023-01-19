module Commands.Dev.Scope.Options where

import CommonOptions
import GlobalOptions
import Juvix.Compiler.Concrete.Pretty qualified as Scoper

data ScopeOptions = ScopeOptions
  { _scopeInputFile :: AppPath File,
    _scopeWithComments :: Bool,
    _scopeListComments :: Bool
  }
  deriving stock (Data)

makeLenses ''ScopeOptions

parseScope :: Parser ScopeOptions
parseScope = do
  _scopeWithComments <-
    switch
      ( long "with-comments"
          <> help "Include user comments when printing code"
      )
  _scopeListComments <-
    switch
      ( long "list-comments"
          <> help "List the user comments"
      )
  _scopeInputFile <- parseInputJuvixFile
  pure ScopeOptions {..}

instance CanonicalProjection (GlobalOptions, ScopeOptions) Scoper.Options where
  project (g, _) =
    Scoper.defaultOptions
      { Scoper._optShowNameIds = g ^. globalShowNameIds,
        Scoper._optNoApe = g ^. globalNoApe
      }
