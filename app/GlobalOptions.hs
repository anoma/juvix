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
    _globalStdin :: Bool,
    _globalNoTermination :: Bool,
    _globalNoPositivity :: Bool,
    _globalNoStdlib :: Bool,
    _globalInputFiles :: [FilePath]
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

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions =
  GlobalOptions
    { _globalNoColors = False,
      _globalShowNameIds = False,
      _globalOnlyErrors = False,
      _globalNoTermination = False,
      _globalStdin = False,
      _globalNoPositivity = False,
      _globalNoStdlib = False,
      _globalInputFiles = []
    }

instance Semigroup GlobalOptions where
  o1 <> o2 =
    GlobalOptions
      { _globalNoColors = o1 ^. globalNoColors || o2 ^. globalNoColors,
        _globalShowNameIds = o1 ^. globalShowNameIds || o2 ^. globalShowNameIds,
        _globalOnlyErrors = o1 ^. globalOnlyErrors || o2 ^. globalOnlyErrors,
        _globalStdin = o1 ^. globalStdin || o2 ^. globalStdin,
        _globalNoTermination = o1 ^. globalNoTermination || o2 ^. globalNoTermination,
        _globalNoPositivity = o1 ^. globalNoPositivity || o2 ^. globalNoPositivity,
        _globalNoStdlib = o1 ^. globalNoStdlib || o2 ^. globalNoStdlib,
        _globalInputFiles = o1 ^. globalInputFiles ++ o2 ^. globalInputFiles
      }

instance Monoid GlobalOptions where
  mempty = defaultGlobalOptions
  mappend = (<>)

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
  return GlobalOptions {_globalInputFiles = [], ..}

genericFromGlobalOptions :: GlobalOptions -> E.GenericOptions
genericFromGlobalOptions GlobalOptions {..} = E.GenericOptions {E._showNameIds = _globalShowNameIds}

commandFirstFile :: GlobalOptions -> Maybe FilePath
commandFirstFile GlobalOptions {..} =
  listToMaybe _globalInputFiles
