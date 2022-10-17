module GlobalOptions
  ( module GlobalOptions,
  )
where

import Juvix.Compiler.Abstract.Pretty.Options qualified as Abstract
import Juvix.Compiler.Internal.Pretty.Options qualified as Internal
import Juvix.Data.Error.GenericError qualified as E
import Juvix.Prelude
import Options.Applicative hiding (hidden)

data GlobalOptions = GlobalOptions
  { _globalNoColors :: Bool,
    _globalShowNameIds :: Bool,
    _globalOnlyErrors :: Bool,
    _globalNoApe :: Bool,
    _globalStdin :: Bool,
    _globalNoTermination :: Bool,
    _globalNoPositivity :: Bool,
    _globalNoStdlib :: Bool
  }
  deriving stock (Eq, Show, Data)

makeLenses ''GlobalOptions

instance CanonicalProjection GlobalOptions Internal.Options where
  project g =
    Internal.Options
      { Internal._optShowNameIds = g ^. globalShowNameIds
      }

instance CanonicalProjection GlobalOptions Abstract.Options where
  project g =
    Abstract.defaultOptions
      { Abstract._optShowNameIds = g ^. globalShowNameIds
      }

instance CanonicalProjection GlobalOptions E.GenericOptions where
  project GlobalOptions {..} =
    E.GenericOptions
      { E._showNameIds = _globalShowNameIds,
        E._genericNoApe = _globalNoApe
      }

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions =
  GlobalOptions
    { _globalNoColors = False,
      _globalShowNameIds = False,
      _globalOnlyErrors = False,
      _globalNoApe = False,
      _globalNoTermination = False,
      _globalStdin = False,
      _globalNoPositivity = False,
      _globalNoStdlib = False
    }

-- | Get a parser for global flags which can be hidden or not depending on
-- the input boolean
parseGlobalFlags :: Parser GlobalOptions
parseGlobalFlags = do
  _globalNoColors <-
    switch
      ( long "no-colors"
          <> help "Disable ANSI formatting"
      )
  _globalShowNameIds <-
    switch
      ( long "show-name-ids"
          <> help "Show the unique number of each identifier when pretty printing"
      )
  _globalNoApe <-
    switch
      ( long "no-format"
          <> help "disable the new pretty printing algorithm"
      )
  _globalStdin <-
    switch
      ( long "stdin"
          <> help "Read from Stdin"
      )
  _globalOnlyErrors <-
    switch
      ( long "only-errors"
          <> help "Only print errors in a uniform format (used by juvix-mode)"
      )
  _globalNoTermination <-
    switch
      ( long "no-termination"
          <> help "Disable termination checking"
      )
  _globalNoPositivity <-
    switch
      ( long "no-positivity"
          <> help "Disable positivity checking for inductive types"
      )
  _globalNoStdlib <-
    switch
      ( long "no-stdlib"
          <> help "Do not use the standard library"
      )
  return GlobalOptions {..}
