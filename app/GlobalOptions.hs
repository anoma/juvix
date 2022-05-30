{-# LANGUAGE ApplicativeDo #-}

module GlobalOptions
  ( module GlobalOptions,
  )
where

import MiniJuvix.Prelude
import Options.Applicative

data GlobalOptions = GlobalOptions
  { _globalNoColors :: Bool,
    _globalShowNameIds :: Bool,
    _globalOnlyErrors :: Bool,
    _globalNoTermination :: Bool
  }
  deriving stock (Eq, Show)

makeLenses ''GlobalOptions

parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions = do
  _globalNoColors <-
    switch
      ( long "no-colors"
          <> help "Disable globally ANSI formatting"
      )
  _globalShowNameIds <-
    switch
      ( long "show-name-ids"
          <> help "Show the unique number of each identifier when pretty printing"
      )
  _globalOnlyErrors <-
    switch
      ( long "only-errors"
          <> help "Only print errors in a uniform format (used by minijuvix-mode)"
      )
  _globalNoTermination <-
    switch
      ( long "no-termination"
          <> help "Disable the termination checker"
      )
  pure GlobalOptions {..}
